library("data.table")

# Describe the potential species that can be caught with a given gear
species_df <- data.frame(gear_type_id = c(50, 50, 50, 50, 51, 51, 51),
                         species_id = c(1, 2, 3, 4, 3, 4, 5),
                         species = c('A', 'B', 'C', 'D', 'C', 'D', 'E'),
                         gear = c('G1', 'G1','G1','G1','G2','G2','G2'))

# Describe the species caught per haul
species_sampling_df <- data.frame(haul_id = c(1, 1, 2, 2 ),
                                  species_id = c(1, 2, 3, 4),
                                  catch = c(1, 0, 1, 0),
                                  discard = c(0, 1, 0, 1))

# Describe the gear types that used in each haul
gears_df <- data.frame(haul_id = c(1, 2),
                       gear_type_id = c(50, 51))

m_species_df <- species_df %>% mutate(species_arte_id=as.numeric(as.factor(paste0(gear_type_id,species_id))))

m_species_sampling_df <- merge(species_sampling_df, gears_df, by="haul_id", all = TRUE)
m_species_sampling_df <- m_species_sampling_df %>% mutate(species_arte_id=as.numeric(as.factor(paste0(gear_type_id,species_id))))

hauls_arte_species_df <- merge(m_species_sampling_df, m_species_df, by="species_arte_id", all.y = TRUE)
