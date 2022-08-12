
# Descrição ---------------------------------------------
#rotina que permite montar através de NFS as pastas shm_data p/ o site
#roitina para montar através de NFS as pastas shm_data_auxi, shiny-server e subroutines
#Antes de correr os comandos, as pastas de destino em shm.lnec.pt tem que ser criadas

# Montar pasta shm_auxi --------------------------------------------------------------
system("echo 4MPNEU8YmFS2 | sudo -S mount nasdenoe1:/volume1/SHM/shm_data_auxi /home/shiny/shm_auxi")

# Montar pasta SHINY-SERVER --------------------------------------------------------------
system("echo 4MPNEU8YmFS2 | sudo -S mount nasdenoe1:/volume1/SHM/shm_lnec_pt /home/shiny/shiny-server")

# Montar pasta SHINY-SERVER www --------------------------------------------------------------
system("echo 4MPNEU8YmFS2 | sudo -S mount nasdenoe1:/volume1/SHM/shm_lnec_pt/WWW/ /home/shiny/shiny-server/WWW/")

# Montar pasta PROC das obra --------------------------------------------------------------
path_matriz_obras <- "/home/shiny/shm_auxi/matriz_obras.RData"
load(path_matriz_obras)
obras_a_inserir <- matriz_obras$Path
for(i in 1:length(obras_a_inserir)){
  obra_i <- obras_a_inserir[i]
  codigo <- paste0("echo 4MPNEU8YmFS2 | sudo -S mount nasdenoe1:/volume1/SHM/shm_data/'",obra_i,"'/PROC /home/shiny/shm_data/'",obra_i,"'/PROC")
  system(codigo)
}


