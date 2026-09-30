import polars as pl
import numpy as np
from multiprocessing import Pool
from tqdm import tqdm
from cvxpy import Minimize, Variable, Problem, ECOS, norm
import gzip
import os
import warnings
warnings.filterwarnings("ignore")

def minim(i):
    global sh_data, sh_covinv, sh_Q
    mask = sh_data[:,0] != sh_data[i,0]
    XNi = sh_data[i,1:]
    XM  = sh_data[mask,1:]

    # Mahalanobis distance of every donor to individual i. This equals
    # diag(quad_form((XNi - XM).T, covinv)) without building the dense
    # (n - 1) x (n - 1) matrix.
    diff = XM - XNi
    dist = np.sqrt(np.einsum("ij,jk,ik->i", diff, sh_covinv, diff))
    x = Variable(mask.sum() , name = "x")

    obj = Minimize(norm(((XNi - x @ XM)@sh_Q).T)+ x @ dist)
    constr  = [x >= 0, x <= 1, sum(x) == 1]

    val = Problem(obj, constr).solve(solver =  ECOS,verbose = False)
    out = x.value.clip(min=0).round(5)
    row = np.zeros(len(sh_data), dtype=np.float32)
    row[mask] = out
    return i, row

def init_worker(data, covinv, Q):
    global sh_data, sh_covinv, sh_Q
    sh_data = data
    sh_covinv  = covinv
    sh_Q  = Q

if __name__ == "__main__":

    social_vars = ["sexo",
                   "edad_anios",
                   "quintil_2",
                   "quintil_3",
                   "quintil_4",
                   "quintil_5",
                   'nivel_escolaridad_primaria',
                   'nivel_escolaridad_secundaria',
                   'nivel_escolaridad_técnica',
                   'nivel_escolaridad_universitaria',
                   "estudia",
                   "trabaja",
                   'macrozona_metropolitana',
                   "horas_trabajo_contratadas",
                    "n_menores_0_4",
                    "n_menores_5_14",
                    "n_personas_15_65",
                    "n_tercera_edad",
                   ]

    data = pl.read_csv("data/raw/ENUT_PRE_WEEKEND_IMPUTATION.csv",
                       infer_schema_length=100000,
                       null_values = "NA")
    data =  (
        data
        .with_columns(quintil=pl.col("quintil").cast(int))
        .to_dummies(columns=["quintil", "macrozona", "nivel_escolaridad"])
        .sort("id_persona")
        .select(["dia_fin_semana"] + social_vars ))

    data = data.to_numpy()
    covar = np.cov(data[:,1:].T)

    covinv = np.linalg.inv(covar)
    Q = np.linalg.cholesky(covinv)

    n = len(data)
    mu = np.zeros((n,n), dtype=np.float32)
    workers = int(os.environ.get("TWIN_WORKERS", os.cpu_count()))
    with Pool(workers, initializer = init_worker, initargs = (data, covinv, Q, )) as p:
        for i, vec in tqdm(p.imap(minim, range(n), chunksize = 8), total=n):
            mu[i] = vec

    # Same headerless %.4f CSV as before, streamed in blocks to bound memory.
    with gzip.open("data/raw/matriz_gemelos.csv.gzip", "wt") as out:
        for start in range(0, n, 500):
            np.savetxt(out, mu[start:start + 500], fmt="%.4f", delimiter=",")

    # for i in tqdm(range(n)):
    #     minim(i)
    # np.savetxt("data tesis/matriz_gemelos.txt", mu.round(5), fmt='%.5f',  delimiter=',')
