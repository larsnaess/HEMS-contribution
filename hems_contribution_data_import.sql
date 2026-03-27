SET NOCOUNT ON;
SET XACT_ABORT ON;
SET ANSI_NULLS ON;
SET QUOTED_IDENTIFIER ON;
SET DATEFIRST 1;

DECLARE @start_date date = '2022-01-01',
        @end_date   date = '2024-12-31';

-- Clean up for multiple executions
IF OBJECT_ID('tempdb..#dl') IS NOT NULL DROP TABLE #dl;
IF OBJECT_ID('tempdb..#hm') IS NOT NULL DROP TABLE #hm;
IF OBJECT_ID('tempdb..#docs') IS NOT NULL DROP TABLE #docs;
IF OBJECT_ID('tempdb..#hp') IS NOT NULL DROP TABLE #hp;
IF OBJECT_ID('tempdb..#ei') IS NOT NULL DROP TABLE #ei;
IF OBJECT_ID('tempdb..#nmi') IS NOT NULL DROP TABLE #nmi;
IF OBJECT_ID('tempdb..#ep') IS NOT NULL DROP TABLE #ep;
IF OBJECT_ID('tempdb..#he') IS NOT NULL DROP TABLE #he;
IF OBJECT_ID('tempdb..#hs') IS NOT NULL DROP TABLE #hs;
IF OBJECT_ID('tempdb..#qibase') IS NOT NULL DROP TABLE #qibase;
IF OBJECT_ID('tempdb..#qi') IS NOT NULL DROP TABLE #qi;

------------------------------------------------------------
-- 1) DUTYLOG (#dl)
------------------------------------------------------------
SELECT
      CAST(dl.[hendelse_pk] AS int)                                     AS dl_incident
    , dl.[År]                                                           AS dl_year
    , CAST(dl.[indeks] AS int)                                          AS dl_index
    , dl.[varslet]                                                      AS dl_alarm
    , dl.[VaktID]                                                       AS dl_resource
    -- , dl.[ressurs_navn1]                                                AS dl_doctor         -- Sensitive info!
    , dl.[kommunenavn]                                                  AS dl_municipality
    , dl.[kode_navn]                                                    AS dl_incident_type
    , ROW_NUMBER() OVER (
          PARTITION BY dl.[hendelse_pk]
          ORDER BY dl.[varslet] DESC
      )                                                                  AS dl_ino
INTO #dl
FROM amisSTO.VaktloggLA AS dl
WHERE dl.[varslet] >= @start_date
  AND dl.[varslet] <  DATEADD(DAY, 1, @end_date);

CREATE NONCLUSTERED INDEX IX_dl_incident_index_ino ON #dl (dl_incident, dl_index, dl_ino);
CREATE NONCLUSTERED INDEX IX_dl_filtered ON #dl (dl_incident, dl_index) INCLUDE (dl_alarm) WHERE dl_ino = 1;

------------------------------------------------------------
-- 2) HEMS MISSION (#hm)
------------------------------------------------------------
  SELECT
    TRY_CAST(
      CASE
        WHEN CHARINDEX('-', mis.[amk_ref_nr]) > 0
        THEN LEFT(mis.[amk_ref_nr], CHARINDEX('-', mis.[amk_ref_nr]) - 1)
        ELSE mis.[amk_ref_nr]
      END AS int
    ) AS hm_incident
  , mis.[reg_aar]                  AS hm_year
  , mis.[base_ref_nr]              AS hm_index
  , TRY_CAST(mis.[oppdrag_id] AS int) AS hm_mission
  , mis.[alarm]                    AS hm_alarm
  , urg.[Hastegrad]                AS hm_urgency
  , mtp.[Oppdragstype]             AS hm_type_raw
  , CASE
      WHEN mtp.[Oppdragstype] IS NULL        THEN NULL
      WHEN mtp.[Oppdragstype] = 'Primær'     THEN 'Primary'
      WHEN mtp.[Oppdragstype] = 'Sekundær'   THEN 'Secondary'
      WHEN mtp.[Oppdragstype] = 'Søk/redning' THEN 'SAR'
      ELSE 'Other'
    END                            AS hm_type_cat
  , CASE
      WHEN mis.[fartoy_type] IS NULL THEN NULL
      WHEN mis.[fartoy_type] = 'H'   THEN 'Helicopter'
      WHEN mis.[fartoy_type] = 'B'   THEN 'RRC'
      ELSE 'Other'
    END                            AS hm_vessel
  , CASE
      WHEN mis.[avvik] IS NULL THEN NULL
      WHEN mis.[avvik] = 1     THEN 0
      WHEN mis.[avvik] = 2     THEN 1
      ELSE NULL
    END                           AS hm_deviation
  , dvt.[Avvikstype]              AS hm_dev_type
  , CASE
      WHEN dvt.[Avvikstype] = 'Avvist'      THEN 'Rejected'
      WHEN dvt.[Avvikstype] = 'Avbrutt'     THEN 'Aborted'
      ELSE 'Completed'
    END                           AS hm_dispatch
  , dvc.[Aarsakskode]             AS hm_dev_cause
  , mis.[avvik_type_annet]        AS hm_dev_com
  , mis.[pers_lege]               AS hm_doc_id
  -- , CONCAT(prs.[navn_etter], ', ', prs.[navn_for]) AS hm_doc_name							-- Sensitive info!
  INTO #hm
  FROM [labas].[tbl_oppdrag] AS mis
  LEFT JOIN [labas].[t_oppdragstype] AS mtp
    ON mis.[rekv_oppdragstype] = mtp.[Type_kode]
   AND mtp.[lokasjon] = 'Rosten'
  LEFT JOIN [labas].[t_hastegrad] AS urg
    ON mis.[rekv_hastegrad] = urg.[Kategori_kode]
   AND urg.[lokasjon] = 'Rosten'
  LEFT JOIN [labas].[t_avvikstype] AS dvt
    ON mis.[avvik_type] = dvt.[Avviktype_kode]
   AND dvt.[lokasjon] = 'Rosten'
  LEFT JOIN [labas].[t_avviksaarsak] AS dvc
    ON mis.[avvik_aarsak] = dvc.[Avvik_aarsak]
   AND dvc.[lokasjon] = 'Rosten'
  LEFT JOIN [labas].[tbl_ansatt] AS emp
    ON mis.[pers_lege] = emp.[ansatt_id]
   AND emp.[base_id]  = 'TRD/H'
   AND emp.[kategori] = 3
   AND emp.[sluttet_dato] IS NULL
  LEFT JOIN [labas].[tbl_person] AS prs
    ON emp.[person_id] = prs.[person_id]
  WHERE mis.[base_id] = 'TRD/H'
    AND mis.[alarm] >= @start_date
    AND mis.[alarm] <  DATEADD(DAY, 1, @end_date)
    AND TRY_CAST(mis.[oppdrag_id] AS int) IS NOT NULL

CREATE NONCLUSTERED INDEX IX_hm_mission        ON #hm (hm_mission);
CREATE NONCLUSTERED INDEX IX_hm_incident       ON #hm (hm_incident);
CREATE NONCLUSTERED INDEX IX_hm_incident_index ON #hm (hm_incident, hm_index);

------------------------------------------------------------
-- 3) Doctor alias (#doc)
------------------------------------------------------------


SELECT hm_doc_id
     , ROW_NUMBER() OVER (ORDER BY doc.hm_doc_id ASC) AS rn
INTO #docs
FROM (
    SELECT DISTINCT hm_doc_id
    FROM #hm
    WHERE hm_doc_id IS NOT NULL
) AS doc;

CREATE NONCLUSTERED INDEX IX_docs_id ON #docs (hm_doc_id);

------------------------------------------------------------
-- 4) HEMS PATIENT (#hp)
------------------------------------------------------------
SELECT
    hp.[oppdrag_id]                                      AS hp_mission
  , hm.[hm_incident]                                     AS hp_incident
  , hm.[hm_alarm]                                        AS hp_alarm
  , hp.[tid_pas_ankomst]                                 AS hp_toa
  , hp.[pasient_id]                                      AS hp_la_pid
  , CASE
        WHEN hp.[pas_kjonn] = 1 THEN 'Male'
        WHEN hp.[pas_kjonn] = 2 THEN 'Female'
        ELSE NULL
    END                                                  AS hp_sex
  , CASE
        WHEN hp.[pas_fodt] > '1900-01-01' THEN CAST(hp.[pas_fodt] AS date)
        ELSE NULL
    END                                                  AS hp_dob

  , CASE
      WHEN hp.[pas_fodt] > '1900-01-01' THEN
          CASE
              WHEN (
                  DATEDIFF(YEAR, hp.[pas_fodt], CAST(hm.[hm_alarm] AS date))
                  - CASE
                      WHEN DATEADD(
                               YEAR,
                               DATEDIFF(YEAR, hp.[pas_fodt], CAST(hm.[hm_alarm] AS date)),
                               hp.[pas_fodt]
                           ) > CAST(hm.[hm_alarm] AS date)
                      THEN 1 ELSE 0
                    END
              ) < 0
              AND DATEDIFF(DAY, hp.[pas_fodt], CAST(hm.[hm_alarm] AS date)) = -1 -- Check to ensure births registered on day after alarm is counted as 0 and not -1
              THEN 0
              ELSE (
                  DATEDIFF(YEAR, hp.[pas_fodt], CAST(hm.[hm_alarm] AS date))
                  - CASE
                      WHEN DATEADD(
                               YEAR,
                               DATEDIFF(YEAR, hp.[pas_fodt], CAST(hm.[hm_alarm] AS date)),
                               hp.[pas_fodt]
                           ) > CAST(hm.[hm_alarm] AS date)
                      THEN 1 ELSE 0
                    END
              )
          END
      ELSE NULL
    END AS hp_age
	
  , CAST(hp.[naca] AS INT)                        		 AS hp_naca
  , CASE
		WHEN CAST(hp.[naca] AS INT) <= 3 THEN 'NACA 1-3'
		WHEN CAST(hp.[naca] AS INT)  = 4 THEN 'NACA 4'
		WHEN CAST(hp.[naca] AS INT)  = 5 THEN 'NACA 5'
		WHEN CAST(hp.[naca] AS INT)  = 6 THEN 'NACA 6'
		WHEN CAST(hp.[naca] AS INT)  = 7 THEN 'NACA 7'
	END 												 AS hp_naca_gr

  , dia.[icd10]                                          AS hp_dia
  , SUBSTRING(dia.[icd10], 1, 3)                         AS hp_dia_3
  , CASE
		WHEN hp.[pas_trans_middel] = 1 THEN 'Transported by HEMS'
		WHEN hp.[pas_trans_middel] = 2 THEN 'Transported by GEMS w/HEMS-physician'
		WHEN hp.[pas_trans_middel] = 3 THEN 'Transported by GEMS w/HEMS-physician'
		WHEN hp.[pas_trans_middel] = 4 THEN 'No HEMS-assisted transport'
	END													 AS hp_transport 
  , CASE
        WHEN hp.[pas_trans_middel] = 1 THEN 'Helicopter'
        WHEN hp.[pas_trans_middel] = 2 THEN 'Ground ambulance'
        WHEN hp.[pas_trans_middel] = 3 THEN 'Other transport'
        WHEN hp.[pas_trans_middel] = 4 THEN 'Not transported by HEMS'
        ELSE NULL
    END                                                  AS hp_mot
  , hp.[tid_pas_trans_start]                             AS hp_tst
  , hp.[tid_pas_trans_stop]                              AS hp_tsp
  -- , dap.[navn]                                           AS hp_dap
  -- , daa.[flyplass]                                       AS hp_daa
  , CASE
        WHEN daa.[flyplass] IN ('Værnes','Vigra') THEN 'Airport'
        WHEN dap.[navn] IS NULL THEN NULL
        WHEN RTRIM(LTRIM(dap.[navn])) IN ('St. Olavs Hospital','St. Olavs Hospital HF')
             THEN 'St. Olavs Hospital'
        ELSE 'Other hospital'
    END                                                  AS hp_delivered
	
	-- Advanced medical procedures (prehospital)
  , aai.[intubert_trach]                                 AS hp_intub_trach
  , aai.[ventilator]                                     AS hp_ventilator
  , aai.[hlr]                                            AS hp_cpr
  , aai.[ekmo]                                           AS hp_ecmo
  , aai.[defib_el_konver]                                AS hp_defib_elconv
  , aai.[thorax_drenasje]                                AS hp_thorax_drain
  , aai.[cvk]                                            AS hp_cvk
  , aai.[transfusjon]                                    AS hp_transfusion
  , aai.[anestesi]                                       AS hp_anesthesia
  , aai.[trombolyse]                                     AS hp_thrombolysis
  , aai.[invasiv_monit]                                  AS hp_inv_mon
  , aai.[kuvose]                                         AS hp_incubator
  , aai.[ext_pacemaker]                                  AS hp_ext_pacem
  , aai.[hjertekompr_mas]                                AS hp_heart_comp_mach
  , aai.[ultralyd]                                       AS hp_ultrasound
  
  
  , (
		CASE WHEN aai.intubert_trach IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.ventilator IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.hlr IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.ekmo IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.defib_el_konver IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.thorax_drenasje IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.cvk IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.transfusjon IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.anestesi IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.trombolyse IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.invasiv_monit IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.kuvose IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.ext_pacemaker IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.hjertekompr_mas IS NOT NULL THEN 1 ELSE 0 END +
        CASE WHEN aai.ultralyd IS NOT NULL THEN 1 ELSE 0 END
    ) AS hp_proc_sum
  
  , CAST(
        ROW_NUMBER() OVER (
            PARTITION BY hp.[oppdrag_id]
            ORDER BY
                hp.[pas_trans_middel] ASC
              , hp.[naca]            DESC
              , hp.[pas_fodt]        DESC
        ) AS INT
    )                                                    AS hp_pno
  , COUNT(hp.[pasient_id]) OVER (PARTITION BY hp.[oppdrag_id]) AS hp_pcn
INTO #hp
FROM [labas].[tbl_pasient] AS hp
INNER JOIN #hm AS hm
    ON hm.[hm_mission] = hp.[oppdrag_id]
LEFT JOIN (
    -- Selecting only one primary diagnosis where there are more in data
    SELECT
          x.[base_id]
        , x.[pasient_id]
        , x.[icd10]
    FROM (
        SELECT
              d.[base_id]
            , d.[pasient_id]
            , d.[icd10]
            , ROW_NUMBER() OVER (PARTITION BY d.[base_id], d.[pasient_id]
                                 ORDER BY (SELECT NULL)) AS rn
        FROM [labas].[tbl_diagnose] AS d
        WHERE d.[primar_diagnose] = 'True'
    ) AS x
    WHERE x.rn = 1
) AS dia
    ON  hp.[pasient_id] = dia.[pasient_id]
    AND hp.[base_id]    = dia.[base_id]
LEFT JOIN (
    SELECT DISTINCT
          inst.[inst_nr]
        , inst.[navn]
    FROM [labas].[th_institusjon] AS inst
    WHERE inst.[skjules] = 'False'
) AS dap
    ON hp.[pas_avlevert_institusjon] = dap.[inst_nr]
LEFT JOIN (
    SELECT DISTINCT
          fp.[icao]
        , fp.[flyplass]
    FROM [labas].[th_flyplass] AS fp
    WHERE fp.[skjules] = 'False'
) AS daa
    ON hp.[pas_avlevert_flyplass] = daa.[icao]
LEFT JOIN [labas].[dwIntervensjonPivotert] AS aai
    ON  hp.[pasient_id] = aai.[pasient_id]
    AND hp.[base_id]    = aai.[base_id]
WHERE hp.[base_id] = 'TRD/H';


CREATE NONCLUSTERED INDEX IX_hp_mission  		ON #hp (hp_mission);
CREATE NONCLUSTERED INDEX IX_hp_incident 		ON #hp (hp_incident);
CREATE NONCLUSTERED INDEX IX_hp_dob      		ON #hp (hp_dob);
CREATE NONCLUSTERED INDEX IX_hp_alarm_naca_num	ON #hp (hp_alarm, hp_naca) INCLUDE (hp_dob);
CREATE NONCLUSTERED INDEX IX_hp_mission_pno1 ON #hp (hp_mission) INCLUDE (hp_dob, hp_alarm, hp_naca) WHERE hp_pno = 1;

------------------------------------------------------------
-- 4) EMCC INCIDENT (#ei)
------------------------------------------------------------
SELECT
      TRY_CAST(ei.[hendelse_pk] AS int)       AS emcc_incident_i
    , ei.[kommunenr]                          AS emcc_municipality_nr
    , ei.[kommunenavn]                        AS emcc_municipality_name
    , ei.[x_kordinat]                         AS emcc_x_coordinate
    , ei.[y_kordinat]                         AS emcc_y_coordinate
    , ei.[kordinatsystem]                     AS emcc_coordinate_sys
    , mun.[FylkeBeskrivelse]                  AS mun_county
INTO #ei
FROM amisQV.Hendelse AS ei
LEFT JOIN Kommune AS mun
  ON ei.[kommunenr] = mun.[KommuneKode]
WHERE ei.[SykehusKode] = 'TR'
  AND EXISTS (
        SELECT 1
        FROM #dl AS dl
        WHERE dl.[dl_incident] = TRY_CAST(ei.[hendelse_pk] AS int)
      );

CREATE NONCLUSTERED INDEX IX_ei_incident ON #ei (emcc_incident_i);

------------------------------------------------------------
-- 5) NMI groups (#nmi)
------------------------------------------------------------
CREATE TABLE #nmi (
   nmi_grp_name_no NVARCHAR(100)
 , nmi_grp_name_en NVARCHAR(100)
 , nmi_grp_agg_en NVARCHAR(100)
);

INSERT INTO #nmi (nmi_grp_name_no, nmi_grp_name_en, nmi_grp_agg_en)
VALUES
    (N'Allergisk reaksjon', N'Allergic reaction', N'Other medical condition'),
    (N'Bestilt oppdrag', N'Transport reservations', N'Transport reservations'),
    (N'Bevisstløs / redusert bevissthet - puster normalt', N'Unresponsive adult/child - breathing normally', N'Unresponsive, breathing normally'),
    (N'Bevisstløs voksen - puster ikke normalt', N'Unresponsive adult - not breathing normally', N'Unresponsive, NOT breathing normally'),
    (N'Bevisstløs voksen / barn - puster normalt', N'Unresponsive adult/child - breathing normally', N'Unresponsive, breathing normally'),
    (N'Bevisstløst barn / nyfødt - puster ikke normalt', N'Unresponsive child/newborn - not breathing normally', N'Unresponsive, NOT breathing normally'),
    (N'Blødning - ikke traumatisk', N'Bleeding - non-traumatic', N'Other incident/injury'),
    (N'Brannskade / skoldeskade / elektrisk skade', N'Burns/scalding/electrical injury', N'Other incident/injury'),
    (N'Brystsmerter / hjertesykdom', N'Chest pain/heart disease', N'Chest pain/heart disease'),
    (N'Diabetes', N'Diabetes', N'Other medical condition'),
    (N'Drukning', N'Drowning', N'Other incident/injury'),
    (N'Dykkerulykke', N'Diving accident', N'Other incident/injury'),
    (N'Dyrebitt / insektstikk / menneskebitt', N'Animal bites/insect stings', N'Other incident/injury'),
    (N'Feber / infeksjon / sepsis', N'Fever/infection/sepsis', N'Other medical condition'),
    (N'Forgiftning - ikke rusrelatert', N'Poisoning - not alcohol/drug related', N'Other incident/injury'),
    (N'Fremmedlegeme', N'Choking/airway obstruction (foreign object in airway)', N'Choking/airway obstruction'),
    (N'Fremmedlegeme i luftveiene', N'Choking/airway obstruction (foreign object in airway)', N'Choking/airway obstruction'),
    (N'Fødsel', N'Labour/childbirth', N'Other incident/injury'),
    (N'Gynekologi / svangerskap', N'Gynaecology/pregnancy', N'Other incident/injury'),
    (N'Hjerneslagsymptomer', N'Possible stroke/altered level of consciousness', N'Suspected stroke'),
    (N'Hodepine', N'Headache', N'Other medical condition'),
    (N'Hypotermi', N'Hypothermia', N'Other incident/injury'),
    (N'Kjemikalier / gasser / CBRNE', N'Chemicals/gasses/CBRNE', N'Other incident/injury'),
    (N'Koronavirus / COVID-19', N'Covid-19', N'Other medical condition'),
    (N'Krampeanfall', N'Seizures/convulsions/fits', N'Other medical condition'),
    (N'Magesmerter / ryggsmerter', N'Abdominal pain/back pain', N'Other medical condition'),
    (N'Mulig dødsfall / krybbedød', N'Possible death/cot death', N'Other medical condition'),
    (N'Mulig hjerneslag / nedsatt bevissthet', N'Possible stroke/altered level of consciousness', N'Suspected stroke'),
    (N'Psykisk lidelse / selvmordsforsøk', N'Mental health problems/suicide attempt', N'Other incident/injury'),
    (N'Pustevansker', N'Breathing problems', N'Breathing problems'),
    (N'Rus / overdose', N'Intoxication/overdose', N'Other incident/injury'),
    (N'Skade', N'Fractures/wounds/minor injuries', N'Minor injury'),
    (N'Skade - brudd / sår / småskade', N'Fractures/wounds/minor injuries', N'Minor injury'),
    (N'Skade - Mulig alvorlig / omfattende', N'Injury - possibly extensive/severe', N'Major injury'),
    (N'Smerter i mage / rygg / ekstremiteter', N'Abdominal pain/back pain', N'Other medical condition'),
    (N'Somatikk - bestilt oppdrag', N'Transport reservations', N'Transport reservations'),
    (N'Stor hendelse / masseskadehendelse', N'Major disaster/mass-casualty incident', N'Major disaster'),
    (N'Sykt barn', N'Sick child', N'Other medical condition'),
    (N'Trafikkskade', N'Road traffic inury', N'Road traffic injury'),
    (N'Uavklart problem', N'Uncertain/unidentified problem', N'Unidentified problem'),
    (N'Urinveier', N'Urinary tract', N'Other medical condition'),
    (N'Vold / mishandling', N'Violence/abuse', N'Other incident/injury'),
    (N'Øre / nese / hals', N'Ear/nose/throat', N'Other medical condition'),
    (N'Øye', N'Eye', N'Other medical condition');


------------------------------------------------------------
-- 6) EMCC PASIENT (#ep)
------------------------------------------------------------
SELECT
      TRY_CAST(ep.[hendelse_pk] AS int)       AS emcc_incident_p
    , ep.[Pasient_k]                          AS emcc_pid
    , ep.[hastegrad_A]                        AS emcc_urgency
    , ep.[indekskriterie_kriterie_nummer]     AS emcc_nmi_num
    , ep.[indekskriterie_kriterie_navn]       AS emcc_nmi_no
    , ep.[indeksgruppe_gruppe_nummer]         AS emcc_nmi_grp_num
    , nmi.[nmi_grp_name_en]          		  AS emcc_nmi_grp_name
	, nmi.[nmi_grp_agg_en]          		  AS emcc_nmi_grp_agg
    , pat.[FinnesIPAS]                        AS pat_pas_first							-- Check to see if patient is in patient register with updated date of death
    , CAST(pat.[Fødselsdato] AS date)         AS pat_dob
    , CAST(pat.[Dødsdato]   AS date)          AS pat_dod
    , CASE
          WHEN pat.[Kjønn] = 'K' THEN 'Female'
          WHEN pat.[Kjønn] = 'M' THEN 'Male'
          ELSE NULL
      END                                     AS pat_sex
	  
-- Selecting the best match for patient ID when multiple patients share the same incident and date of birth
	, ROW_NUMBER() OVER (
      PARTITION BY TRY_CAST(ep.[hendelse_pk] AS int)
                 , CAST(pat.[Fødselsdato] AS date)
      ORDER BY
          CASE WHEN pat.[FinnesIPAS] IS NOT NULL THEN 0
		  ELSE 1
		  END ASC
        , CASE WHEN ep.[Pasient_k] IS NOT NULL AND ep.[Pasient_k] <> 0 THEN 0
		ELSE 1
		END ASC
      )                                       AS ep_pno

INTO #ep
FROM [amisQV].[PasientKontakt] AS ep

LEFT JOIN [pasient].[PasientAnonym] AS pat
  ON ep.[Pasient_k] = pat.[Pasient_k]

LEFT JOIN #nmi AS NMI
  ON ep.[indeksgruppe_gruppe_tekst] = nmi.[nmi_grp_name_no]

-- Limiting data to incidents found in dutylog
WHERE ep.[SykehusKode] = 'TR'
  AND EXISTS (
        SELECT 1
        FROM #dl AS dl
        WHERE dl.[dl_incident] = TRY_CAST(ep.[hendelse_pk] AS int)
      )
-- Limiting data to patients found in hems patient	  
  AND EXISTS (
        SELECT 1
        FROM #hp AS hp
        WHERE hp.[hp_dob] = CAST(pat.[Fødselsdato] AS date)
      );


CREATE NONCLUSTERED INDEX IX_ep_incident ON #ep (emcc_incident_p);
CREATE NONCLUSTERED INDEX IX_ep_incident_dob ON #ep (emcc_incident_p, pat_dob);
CREATE NONCLUSTERED INDEX IX_ep_inc_dob_pno ON #ep (emcc_incident_p, pat_dob, ep_pno) INCLUDE (emcc_pid);
CREATE NONCLUSTERED INDEX IX_ep_incident_dob_pno1 ON #ep (emcc_incident_p, pat_dob)INCLUDE (emcc_pid) WHERE ep_pno = 1;

------------------------------------------------------------
-- 7) HOSPITAL EPISODE (#he)
------------------------------------------------------------
SELECT
    he.[pasient_k]                               AS he_pid
  -- , CAST(he.[EpisodeOid]     AS INT)             AS he_eid										-- Redundant hospital episode ID
  -- , CAST(he.[eOppholdOid]    AS INT)             AS he_oid										-- Redundant hospital stay ID
  , CAST(he.[NimesOppholdId] AS INT)             AS he_nid										-- Hospital stay ID
  , CAST(he.[InnDatoTid]     AS DATETIME)        AS he_in
  , CAST(he.[UtDatoTid]      AS DATETIME)        AS he_out
  , he.[hastegradBeskrivelse]                    AS he_urgency
  , he.[omsorgsnivåBeskrivelse]                  AS he_loc
  , he.[HovedtilstandKoder]                      AS he_main_dia
  , SUBSTRING(he.[HovedtilstandKoder], 1, 3)     AS he_main_dia_3
  , he.[AndreTilstanderKoder]                    AS he_oth_dia
  , he.[Prosedyrer]                              AS he_proc
  , he.[Ncmpkoder]                               AS he_ncmp

INTO #he
FROM [nimes].[VisEpisodeSTO] AS he
WHERE he.[InnDatoTid] >= @start_date
  AND he.[InnDatoTid] < DATEADD(DAY, 1, @end_date)
  AND he.[UtDatoTid] IS NOT NULL
  AND he.[NimesOppholdId] IS NOT NULL
  AND NOT (he.[HastegradVedAnkomst] = 4 AND he.[NpkOmsorgNivå] = 3)
  AND he.[ErPHVEllerTSB] = 'False'
  AND EXISTS (
        SELECT 1
        FROM #ep AS ep
       WHERE ep.ep_pno   = 1
         AND ep.emcc_pid = he.[pasient_k]
      );


CREATE NONCLUSTERED INDEX IX_he_pid     ON #he(he_pid);
CREATE NONCLUSTERED INDEX IX_he_nid     ON #he(he_nid);
CREATE NONCLUSTERED INDEX IX_he_in_out ON #he (he_in, he_out) INCLUDE (he_pid, he_nid);


------------------------------------------------------------
-- 8) HOSPITAL STAY (#hs)
------------------------------------------------------------
SELECT
    hs.[pasient_k]                                	AS hs_pid
  -- , CAST(hs.[OppholdOid] AS INT)                 	AS hs_oid											-- Redundant hospital stay ID
  , CAST(hs.[NimesOppholdId] AS INT)             	AS hs_nid											-- Hospital stay ID
  , CAST(hs.[InnDatoTid] AS DATETIME)            	AS hs_in
  , CAST(hs.[UtDatoTid] AS DATETIME)             	AS hs_out
  , hs.[behandlingssted]                          	AS hs_location
  , hs.[avdRESHtekst]                             	AS hs_department
  , hs.[fagRESHTekst]                             	AS hs_specialty
  , hs.[UtTilstand]                               	AS hs_discharge_state
  , hs.[LOSDager]                                 	AS hs_days
  , hs.[Poengsum]                                 	AS hs_drg_points
  , hs.[DRGNavn]                                  	AS hs_drg_name
  , hs.[DRGId]                                    	AS hs_drg_id
  , hs.[HovedtilstandKoder]                       	AS hs_main_dia
  , SUBSTRING(hs.[HovedtilstandKoder], 1, 3)        AS hs_main_dia_3
  , hs.[AndreTilstanderKoder]                    	AS hs_oth_dia
  , hs.[AlleTilstander]								AS hs_all_dia
  , hs.[Prosedyrer]                               	AS hs_proc
INTO #hs
FROM [nimes].[VisOppholdSTO] AS hs
WHERE hs.[NimesOppholdId] IN (SELECT he_nid FROM #he)
  AND hs.[ErPHVellerTSB] = 'False';

CREATE NONCLUSTERED INDEX IX_hs_nid ON #hs(hs_nid);
CREATE NONCLUSTERED INDEX IX_hs_pid ON #hs(hs_pid);


------------------------------------------------------------
-- 9) QI-data
------------------------------------------------------------
SELECT
    reg.AmisId AS qi_incident
  -- , CONCAT(doc.Firstname, ' ', doc.Lastname) AS qi_doc				-- Sensitive info!
  -- , reg.PatientId AS qi_patient										-- Possible multiple patients per incident
  -- , reg.Gender AS qi_gender											-- Usable for hp matching, but too general for accurate linkage; redundant since sex and age are available elsewhere
  -- , FLOOR(reg.Age / 12.0) AS qi_age									-- Usable for hp matching, but too general for accurate linkage; redundant since sex and age are available elsewhere
  , qiop.Code AS qi_answer_code
INTO #qibase
FROM NAC.Registrations AS reg
LEFT JOIN NAC.RegistrationQualityIndicatorOptions AS rgop
    ON reg.Id = rgop.RegistrationId
LEFT JOIN NAC.QualityIndicatorOptions AS qiop
    ON rgop.QualityIndicatorOptionId = qiop.Id
LEFT JOIN NAC.QualityIndicators AS qi
    ON qiop.QualityIndicatorId = qi.Id
LEFT JOIN NAC.Doctors AS doc
    ON reg.DoctorId = doc.Id
WHERE reg.AmisId IS NOT NULL
  -- AND reg.StateId = 2												-- Possible filter for selecting only completed registrations; due to technical issues, some registrations are marked as unfinished
  AND qi.Code IN (8, 24, 25)
  AND qiop.Id <> 53;

CREATE INDEX IX_qibase_key ON #qibase (qi_incident);
CREATE INDEX IX_qibase_code ON #qibase (qi_answer_code);

SELECT
    qib.qi_incident

  -- Patient contact
  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code = '8.1' THEN 1 ELSE 0 END) = 1 THEN 'Yes'
        WHEN MAX(CASE WHEN qib.qi_answer_code = '8.2' THEN 1 ELSE 0 END) = 1 THEN 'No'
        ELSE NULL
    END AS qi_patient_contact

  -- Clinical contribution
  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code = '24.1' THEN 1 ELSE 0 END) = 1 THEN 'Yes'
        WHEN MAX(CASE WHEN qib.qi_answer_code = '24.2' THEN 1 ELSE 0 END) = 1 THEN 'No'
        ELSE NULL
    END AS qi_clinical_contribution

  -- Logistical contribution
  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code = '25.1' THEN 1 ELSE 0 END) = 1 THEN 'Yes'
        WHEN MAX(CASE WHEN qib.qi_answer_code = '25.2' THEN 1 ELSE 0 END) = 1 THEN 'No'
        ELSE NULL
    END AS qi_logistical_contribution

  -- Subsections for 24 (clinical contribution)
  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code IN ('24.1','24.2') THEN 1 ELSE 0 END) = 1
             THEN CASE WHEN MAX(CASE WHEN qib.qi_answer_code = '24.1.1' THEN 1 ELSE 0 END) = 1 THEN 'Yes' ELSE 'No' END
        ELSE NULL
    END AS qi_la_procedures

  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code IN ('24.1','24.2') THEN 1 ELSE 0 END) = 1
             THEN CASE WHEN MAX(CASE WHEN qib.qi_answer_code = '24.1.2' THEN 1 ELSE 0 END) = 1 THEN 'Yes' ELSE 'No' END
        ELSE NULL
    END AS qi_other_procedures

  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code IN ('24.1','24.2') THEN 1 ELSE 0 END) = 1
             THEN CASE WHEN MAX(CASE WHEN qib.qi_answer_code = '24.1.3' THEN 1 ELSE 0 END) = 1 THEN 'Yes' ELSE 'No' END
        ELSE NULL
    END AS qi_defer_treatment

  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code IN ('24.1','24.2') THEN 1 ELSE 0 END) = 1
             THEN CASE WHEN MAX(CASE WHEN qib.qi_answer_code = '24.1.4' THEN 1 ELSE 0 END) = 1 THEN 'Yes' ELSE 'No' END
        ELSE NULL
    END AS qi_difficult_situation

  -- Subsections for 25 (logistical contribution)
  , CASE
		WHEN MAX(CASE WHEN qib.qi_answer_code IN ('25.1','25.2') THEN 1 ELSE 0 END) = 1
			THEN CASE
				WHEN MAX(CASE WHEN qib.qi_answer_code = '25.1.1.1' THEN 1 ELSE 0 END) = 1
					 THEN N'≥30minutes'
				WHEN MAX(CASE WHEN qib.qi_answer_code = '25.1.1.2' THEN 1 ELSE 0 END) = 1
					 THEN N'<30minutes'
				ELSE N'No'
			  END
		ELSE NULL
	END AS qi_time_gain
	
  , CASE
        WHEN MAX(CASE WHEN qib.qi_answer_code IN ('25.1','25.2') THEN 1 ELSE 0 END) = 1
             THEN CASE WHEN MAX(CASE WHEN qib.qi_answer_code = '25.1.2' THEN 1 ELSE 0 END) = 1 THEN 'Yes' ELSE 'No' END
        ELSE NULL
    END AS qi_inaccessible

INTO #qi
FROM #qibase AS qib
GROUP BY qib.qi_incident; 			-- Limits data to one case per incident and aggregates qi scores in cases with multiple patients

------------------------------------------------------------
-- 10) MAIN QUERY (chain hp → ep → he via APPLY)
------------------------------------------------------------
SELECT

	  CAST(ROW_NUMBER() OVER (ORDER BY dl.dl_incident) AS INT)			AS case_num
	
	, CASE
        WHEN hm.hm_dispatch = 'Completed'
			AND hp.hp_la_pid IS NOT NULL
     AND (
          (hp.hp_naca IS NOT NULL AND hp.hp_dia IS NOT NULL)
          OR (
              (hp.hp_naca IS NULL OR hp.hp_dia IS NULL)
              AND qi_patient_contact = 'Yes'
             )
         )
				THEN 1
				ELSE 0
      END 																AS eligible
	  
	, CASE
        WHEN hm.hm_dispatch = 'Completed'
			AND hp.hp_la_pid IS NOT NULL
     AND (
          (hp.hp_naca IS NOT NULL AND hp.hp_dia IS NOT NULL)
          OR (
              (hp.hp_naca IS NULL OR hp.hp_dia IS NULL)
              AND qi_patient_contact = 'Yes'
             )
         )
		 
		 AND (qi_logistical_contribution IS NOT NULL OR qi_clinical_contribution IS NOT NULL)
				THEN 1
				ELSE 0
      END 																AS included
	  
	, dl.dl_incident													AS incident_num
	, dl.dl_alarm														AS alarm
	, dl.dl_year														AS alarm_year
	, CASE 
        WHEN MONTH(dl.dl_alarm) IN (3, 4, 5)  THEN 'Spring (Mar-May)'
        WHEN MONTH(dl.dl_alarm) IN (6, 7, 8)  THEN 'Summer (Jun-Aug)'
        WHEN MONTH(dl.dl_alarm) IN (9, 10, 11) THEN 'Autumn (Sep-Nov)'
        WHEN MONTH(dl.dl_alarm) IN (12, 1, 2) THEN 'Winter (Dec-Feb)'
        ELSE 'Unknown'
      END                                                               AS alarm_season
	, DATENAME(MONTH, dl.dl_alarm) 										AS alarm_month
	-- , MONTH(dl.dl_alarm)                                            		AS alarm_month_num						-- Redundant info
	, DATENAME(WEEKDAY, dl.dl_alarm) 									AS alarm_weekday
	-- , (((DATEPART(WEEKDAY, dl.dl_alarm) + @@DATEFIRST - 2) % 7) + 1) 	AS alarm_weekday_num					-- Redundant info
	, DATEPART(hour, dl.dl_alarm)                                     	AS alarm_hour
	, CASE 
        WHEN DATEPART(hour, dl.dl_alarm) < 6  THEN '0000-0600'
        WHEN DATEPART(hour, dl.dl_alarm) < 12 THEN '0600-1200'
        WHEN DATEPART(hour, dl.dl_alarm) < 18 THEN '1200-1800'
        ELSE '1800-2400'
      END                                                                AS alarm_shift
	, CASE
        WHEN (((DATEPART(WEEKDAY, dl.dl_alarm) + @@DATEFIRST - 2) % 7) + 1) = 5
             AND DATEPART(hour, dl.dl_alarm) >= 18 THEN 'Weekend'
        WHEN (((DATEPART(WEEKDAY, dl.dl_alarm) + @@DATEFIRST - 2) % 7) + 1) IN (6, 7) THEN 'Weekend'
        WHEN (((DATEPART(WEEKDAY, dl.dl_alarm) + @@DATEFIRST - 2) % 7) + 1) = 1
             AND DATEPART(hour, dl.dl_alarm) < 6 THEN 'Weekend'
        ELSE 'Weekday'
      END                                                               AS alarm_weekpart
  , dl.dl_municipality
  , dl.dl_incident_type
  , dl.dl_resource
  , CASE docs.rn
        WHEN 1 THEN 'doc_a'
        WHEN 2 THEN 'doc_b'
        WHEN 3 THEN 'doc_c'
        WHEN 4 THEN 'doc_d'
        WHEN 5 THEN 'doc_e'
        WHEN 6 THEN 'doc_f'
        WHEN 7 THEN 'doc_g'
        WHEN 8 THEN 'doc_h'
        ELSE CONCAT('doc_', docs.rn)
    END                                                      AS hm_doc_label
  -- , dl.dl_doctor
  , qi.*
  , hm.*
  , hp.*
  
  , CASE 
      WHEN [hp_age] BETWEEN 0 AND 17 THEN '0-17 years'
      WHEN [hp_age] BETWEEN 18 AND 49 THEN '18-49 years'
      WHEN [hp_age] BETWEEN 50 AND 66 THEN '50-66 years'
      WHEN [hp_age] BETWEEN 67 AND 79 THEN '67-79 years'
      WHEN [hp_age] BETWEEN 80 AND 89 THEN '80-89 years'
      WHEN [hp_age] >=90 THEN 'o/90 years'
      ELSE NULL 
    END AS [hp_age_gr]
	
  , CASE
		WHEN hp.hp_naca = 7 							THEN 'No hospital admission'
		WHEN hp.hp_delivered = 'St. Olavs Hospital'
			OR hp.hp_delivered = 'Other hospital'
			OR hp.hp_delivered = 'Airport'
			OR he_sel.he_pid IS NOT NULL
														THEN 'Admitted to hospital'
		WHEN hp.hp_la_pid IS NOT NULL
			AND hp.hp_delivered IS NULL
			AND he_sel.he_pid IS NULL
														THEN 'No hospital admission'
														ELSE NULL
	END 
																			AS hp_outcome
  , CASE
		WHEN hp.hp_naca = 7 													THEN 1
		WHEN DATEDIFF(DAY, CAST(dl.dl_alarm AS DATE), ep.pat_dod) <= 30 		THEN 1
		WHEN ep.pat_pas_first IS NOT NULL										THEN 0
	END																		AS pat_d30
  , ei.*
  , ep.*
  , he_sel.*
  , hs.*
FROM #dl AS dl

LEFT JOIN #hm AS hm
  ON dl.dl_incident = hm.hm_incident
 AND dl.dl_index    = hm.hm_index

LEFT JOIN #docs AS docs
  ON hm.hm_doc_id  = docs.hm_doc_id

LEFT JOIN #hp AS hp
  ON hm.hm_mission  = hp.hp_mission
 AND hp.hp_pno      = 1

LEFT JOIN #ei AS ei
  ON dl.dl_incident = ei.emcc_incident_i

LEFT JOIN #ep AS ep
  ON dl.dl_incident = ep.emcc_incident_p
 AND hp.hp_dob      = ep.pat_dob
 AND ep.ep_pno      = 1

-- Code for matching HEMS data with hospital data across different time windows in a prioritised order
OUTER APPLY (
  SELECT TOP (1) he.*
  FROM #he AS he
  WHERE he.he_pid = ep.emcc_pid
    AND ep.emcc_incident_p = dl.dl_incident
    AND ep.pat_dob = hp.hp_dob
    AND ep.ep_pno = 1													-- Picking most relevant patient match from emcc patient data
    AND hp.hp_alarm < he.he_out
    AND (
          DATEDIFF(HOUR, hp.hp_alarm, he.he_in) BETWEEN -12 AND 12
          OR ((hp.hp_alarm BETWEEN he.he_in AND he.he_out)
              AND DATEDIFF(HOUR, hp.hp_alarm, he.he_out) > 12)
        )
    AND hp.hp_naca <> '7'												-- Dead on scene or in HEMS care, no probable hospital admission
  ORDER BY
    CASE WHEN hp.hp_alarm <= he.he_in THEN 0 ELSE 1 END,
    ABS(DATEDIFF(HOUR, hp.hp_alarm, he.he_in)) ASC,
    he.he_out DESC
) AS he_sel

LEFT JOIN #hs AS hs
  ON he_sel.he_nid = hs.hs_nid

LEFT JOIN #qi AS qi
  ON dl.dl_incident = qi.qi_incident

WHERE dl.dl_ino = 1

ORDER BY dl.dl_incident;

DROP TABLE IF EXISTS #qi;
DROP TABLE IF EXISTS #qibase;
DROP TABLE IF EXISTS #hs;
DROP TABLE IF EXISTS #he;
DROP TABLE IF EXISTS #ep;
DROP TABLE IF EXISTS #nmi;
DROP TABLE IF EXISTS #ei;
DROP TABLE IF EXISTS #hp;
DROP TABLE IF EXISTS #docs;
DROP TABLE IF EXISTS #hm;
DROP TABLE IF EXISTS #dl;
