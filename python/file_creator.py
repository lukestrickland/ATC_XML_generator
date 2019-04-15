import glob
import shutil
import os

os.chdir("..")

n_files_one = len([f for f in glob.glob("XML" +  '/ppt' + str(1) + "_**.xml", recursive=True)])
n_files_all = len([f for f in glob.glob("XML" + "**/*.xml", recursive=True)])
n_ppts = n_files_all / n_files_one 

for i in range(1, int(n_ppts)+1):
    shutil.copytree("ATC-LAB", "ppt_files/p" + str(i))
    file_dirs = [f for f in glob.glob("XML" +  '/ppt' + str(i) + "_**.xml", recursive=True)]
    file_names = [fdir[4:] for fdir in file_dirs]
    [shutil.copyfile(fdir, "ppt_files/p" + str(i) + "/"+ fnam) for fdir, fnam in zip(file_dirs, file_names)]
    #get IVs as well
    IV_dirs = [f for f in glob.glob("data/**" +  '_p' + str(i) + "_**.csv", recursive=True)]
    IV_names = [fdir[5:] for fdir in IV_dirs]
    [shutil.copyfile(fdir, "ppt_files/p" + str(i) + "/IVs/"+ fnam) for fdir, fnam in zip(IV_dirs, IV_names)]