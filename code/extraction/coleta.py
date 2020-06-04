#!/usr/bin/python3
import psycopg2
import re
import pandas as pd
import numpy as np

CONFIG_FILE = "./code/extraction/extract_config.txt"
DATA_DIR = "./data_raw/amparo_2018/"

class Configuração:
    def __init__(self, arqcfg):
        with open(arqcfg, 'r') as file:
            self.specs = {"groupCode": file.readline().split()}
            self.config = file.readline().split()
            
    def specs(self):
        return self.specs
        
    def keys(self):
        return self.specs.keys()
        
    def val(self, k):
        return self.specs[k]
        
    def config(self):
        return self.config
        
    def load(self, df):
        for key in self.config:
            if not(key in df):
                df.insert(0, key, np.nan)
        return df[self.config]
        

conf = Configuração(CONFIG_FILE)

log = open("./logs/erros.txt", "a")

def Erro(msg,dados):
    log.write("\n---------------------------\n")
    log.write(msg+"\n")
    log.write("\n".join(map(str,dados)))
    
def gera_csv(s):
    pid, xyz, dados = s
    if(not(dados)):
        Erro("Dados vazios","")
        return False
    linhas = dados.replace("\r","").split("\n")
    # tabela tem mais de 3 campos e meta apenas 2 ou menos.
    #
    # as vezes os dados contem uma mini-tabela com 3 campos
    # em complemento a tabela grande.
    # esta mini-tabela eh excluida.
    num_cols = [len(re.split("[,:]", linha)) for linha in linhas if linha]
    if(max(num_cols) <= 3):
        Erro("Tabela extra",s)
        return False
    
    # separa meta-dados e transforma em dicionario
    sep = next(idx for idx, item in enumerate(num_cols) if item > 2)
    meta = linhas[:sep]
    try:
        meta = dict(re.split("[,:]", item, 1) for item in meta if item)
    except:
        Erro("Formato inválido",s)
        print("Formato inválido")
        return False
    
    # verifica se meta-dados tem especificacoes desejadas
    for key in conf.keys():
        if(not(key in meta.keys())):
            Erro("Fora de especificação",s)
            return False
        elif(not(meta[key] in conf.val(key))):
            Erro("Não contém espec. desejada", s)
            return False
    
    # cria dataframe com tabela principal em dados
    sep = next(idx for idx, item in enumerate(num_cols) if item > 3)
    df = linhas[sep:]
    header = df[0].split(",")
    df = pd.DataFrame.from_records([linha.split(",") for linha in df[1:] if linha], 
    columns = header)
    
    # insere meta-dados como colunas repetidas em df
    for key in meta.keys():
      df.insert(0, key, meta[key])
    
    df = conf.load(df)
    fname = DATA_DIR + str(pid) + "-"+xyz+".csv"
    df.to_csv(fname)
    return True

con = psycopg2.connect(host='200.144.254.136', port=54321,
                           database='goleiro', user='goleiro',
                           password='gol-00-gol')
cur = con.cursor()
sql = "select * from results"
cur.execute(sql)
res = cur.fetchall()
con.close()

for r in res:
  pid, xyz, dados = r
  print(str(pid) + ": ", end='')
  if(gera_csv(r)):
    print("ok")
  else:
    print("Falhou")

log.close()

# Local variables:
# mode: python
# End:
