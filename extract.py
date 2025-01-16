import time
from urllib.parse import quote_plus
import pandas as pd
from sqlalchemy import create_engine


conexao = (
    'DRIVER={ODBC Driver 17 for SQL Server};'
    'SERVER=DESKTOP-756BP5B\\MSSQLSERVER01;'
    'PORT=1433;'
    'DATABASE=faturamento;'
    'Trusted_Connection=yes;'  # Use Windows Authentication
)

tempo_inicial = time.time()

engine = create_engine("mssql+pyodbc:///?odbc_connect=%s" % quote_plus(conexao))

sql_query =  """
   SELECT [payment_id]
      ,[payment_created_at]
      ,[payment_updated_at]
      ,[payment_clinic_id]
      ,[payment_lioOrderId]
      ,[payment_patient_balance_id]
      ,[payment_date]
      ,[payment_specialty_id]
      ,[payment_user_name]
      ,[payment_specialty_name]
      ,[payment_patient_id]
      ,[payment_patient_name]
      ,[payment_receptionist_id]
      ,[payment_sign_off]
      ,[payment_sign_off_date]
      ,[payment_amount]
      ,[payment_payment_type_id]
      ,[payment_payment_type_name]
      ,[payment_instalments]
      ,[payment_batch_identifier]
      ,[payment_is_paid_by_card]
      ,[payment_due_date]
      ,[payment_to_account_id]
      ,[payment_to_account_name]
      ,[payment_is_confirmed]
      ,[payment_confirmedAt]
      ,[payment_treatment_id]
      ,[patient_id]
      ,[patient_created_at]
      ,[patient_updated_at]
      ,[patient_clinic_id]
      ,[patient_firstName]
      ,[patient_lastName]
      ,[patient_name]
      ,[patient_socialNumber]
      ,[patient_phoneNumber]
      ,[patient_email]
      ,[patient_dateOfBirth]
      ,[patient_streetName]
      ,[patient_streetNumber]
      ,[patient_neighbourhood]
      ,[patient_zipCode]
      ,[patient_nationality]
      ,[patient_sex]
      ,[patient_city]
      ,[patient_public_id]
      ,[patient_channel_type]
      ,[patient_patientBalanceId]
      ,[patient_anamneseId]
      ,[patient_is_new]
      ,[patient_had_avaliaco]
      ,[patient_avaliaco_date]
      ,[patient_avaliaco_appointment_id]
      ,[patient_has_valid_cpf]
      ,[patient_invalid_cpf_reason]
      ,[patient_has_email]
      ,[patient_slipId]
      ,[patient_ortho_notes]
      ,[patient_internalsObservation]
      ,[patient_state]
      ,[patient_complement]
      ,[patient_isDentalis]
      ,[patient_dentalisId]
      ,[patient_dentalisRegistrationDate]
      ,[patient_dentalisPlan]
      ,[patient_dentalisJoined]
      ,[patient_dentalisBasePatientId]
      ,[patient_dentalisSpecialties]
      ,[patient_foreignerWithoutCpf]
      ,[patient_foreignerAuthReqAccepted]
      ,[patient_isFullyMigrated]
      ,[patient_checked]
      ,[patient_financialResponsibleId]
  FROM [faturamento].[dbo].[patient_payment]
"""
c = 0


chunk_size = 1000
for chunk_df in pd.read_sql(sql_query, engine, chunksize=chunk_size):
    # Processar cada pedaço conforme necessário
    chunk_df.to_csv(r'D:/pythonDSA/USP/TCC/extract.csv', encoding='utf-8-sig', index=False,
                    sep=';', mode='a', header=False)
    c = c + 1
    print(c)

tempo_final = time.time()
tempo_execucao = tempo_final - tempo_inicial
print(f"Tempo de execução: {tempo_execucao} segundos")
