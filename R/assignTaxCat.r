##  assignTaxCat.r
##  Created SSR 03/11/2010

##  Assigning taxonomic category based on crew entry
##  This is variable from one study to another, and needs to
##  be updated to match current datasets.


assignTaxCat <- function(lt)
{
lt$taxCat <- NA

#  Deciduous species
lt$taxCat <- ifelse(lt$SPECIES=='ACACIA', 'Acacia/Mesquite', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='MESQUITE', 'Acacia/Mesquite', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='ALDER' |
                     lt$SPECIES=='ALDER/BIRCH' |
                     lt$SPECIES=='BIRCH' |
                     lt$SPECIES=='RIVER BIRCH' |
                     lt$SPECIES=='YELLOW BIRCH', 'Alder/Birch', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='ASH' |
                     lt$SPECIES=='GREEN ASH' |
                     lt$SPECIES=='WHITE ASH' |
                     lt$SPECIES=='ASH-BASSWOOD', 'Ash', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='MAPLE' |
                     lt$SPECIES=='RED MAPLE' |
                     lt$SPECIES=='MAPLE/BOXELDER' |
                     lt$SPECIES=='SUGAR MAPLE' |
                     lt$SPECIES=='SILVER MAPLE' |
                     lt$SPECIES=='BIG TOOTHED MAPLE' |
                     lt$SPECIES=='ACER RUBRUM' |
                     lt$SPECIES=='ACER NEGUNDO' |
                     lt$SPECIES=='ELDER/MAPLE' |
                     lt$SPECIES=='BOXELDER' |
                     lt$SPECIES=='ELDER', 'Maple/Boxelder', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='OAK' |
                     lt$SPECIES=='POST OAK' |
                     lt$SPECIES=='WILLOW OAK' |
                     lt$SPECIES=='WATER OAK' |
                     lt$SPECIES=='WHITE OAK' |
                     lt$SPECIES=='RED OAK' |
                     lt$SPECIES=='OAK (GAMBEL)' |
                     lt$SPECIES=='BEECH (OAK FAMILY)' |
                     lt$SPECIES=='NORTHERN RED OAK' |
                     lt$SPECIES=='CHESTNUT OAK' |
                     lt$SPECIES=='SOUTHERN RED OAK' |
                     lt$SPECIES=='PIN OAK', 'Maple/Boxelder', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='POPLAR/COTTONWOOD' |
                     lt$SPECIES=='POPLAR' |
                     lt$SPECIES=='TULIP POPLAR' |
                     lt$SPECIES=='YELLOW POPLAR' |
                     lt$SPECIES=='ASPEN' |
                     lt$SPECIES=='OTHER (BIG TOOTH ASPEN)' |
                     lt$SPECIES=='OTHER (ASPEN)' |
                     lt$SPECIES=='COTTONWOOD' |
                     lt$SPECIES=='NARROWLEAF COTTONWOOD' |
                     lt$SPECIES=='FREMONT COTTONWOOD' |
                     lt$SPECIES=='EASTERN COTTONWOOD', 'Poplar/Cottonwood', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='SYCAMORE' |
                     lt$SPECIES=='ARIZONA SYCAMORE' |
                     lt$SPECIES=='PLATANUS OCCIDENTALIS', 'Sycamore', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='WILLOW' |
                     lt$SPECIES=='BLACK WILLOW' |
                     lt$SPECIES=='BUTTON WILLOW' |
                     lt$SPECIES=='SALIX SP.' |
                     lt$SPECIES=='GOODING WILLOW', 'Willow', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='OTHER DECIDUOUS' |
                     lt$SPECIES=='UNKNOWN DECIDUOUS' |
                     lt$SPECIES=='WESTERN LARCH' |
                     lt$SPECIES=='UNKNOWN OR OTHER DECIDUOUS' |
                     (lt$SPECIES=='OTHER' & lt$TREE_TYP=='Deciduous') |
                     (lt$SPECIES=='UNKNOWN' & lt$TREE_TYP=='Deciduous'), 'Unknown or Other Deciduous', lt$taxCat)
#  All others marked as deciduous left get lumped into 'Unknown or Other Deciduous'
lt$taxCat <- ifelse(is.na(lt$taxCat) & lt$TREE_TYP=='Deciduous', 'Unknown or Other Deciduous', lt$taxCat)

# Coniferous species
lt$taxCat <- ifelse(lt$SPECIES=='CEDAR' |
                     lt$SPECIES=='WHITE CEDAR' |
                     lt$SPECIES=='WESTERN RED CEDAR' |
                     lt$SPECIES=='BALD CYPRESS' |
                     lt$SPECIES=='CYPRESS' |
                     lt$SPECIES=='POND CYPRESS' |
                     lt$SPECIES=='SEQUOIA', 'Cedar/Cypress/Sequoia', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='FIR' |
                     lt$SPECIES=='DOUGLAS FIR' |
                     lt$SPECIES=='SUBALPINE FIR' |
                     lt$SPECIES=='HEMLOCK' |
                     lt$SPECIES=='EASTERN HEMLOCK' |
                     lt$SPECIES=='FIR/HEMLOCK', 'Firs/Hemlock', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='JUNIPER' |
                     lt$SPECIES=='JUNIPER (NEARLY DEAD)' |
                     lt$SPECIES=='UTAH JUNIPER' |
                     lt$SPECIES=='ALLIGATOR JUNIPER' |
                     lt$SPECIES=='PINYON JUNIPER', 'Juniper', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='PINE' |
                     lt$SPECIES=='WHITE PINE' |
                     lt$SPECIES=='RED PINE' |
                     lt$SPECIES=='LOBLOLLY PINE' |
                     lt$SPECIES=='LODGEPOLE PINE' |
                     lt$SPECIES=='PONDEROSA PINE', 'Pine', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='SPRUCE' |
                     lt$SPECIES=='ENGELMANN SPRUCE' |
                     lt$SPECIES=='BLUE SPRUCE', 'Spruce', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='OTHER CONIFER' |
                     (lt$SPECIES=='UNKNOWN' & lt$TREE_TYP=='Coniferous') |
                     (lt$SPECIES=='UNKNOWN CONIFER' & lt$TREE_TYP=='Coniferous'), 'Unknown or Other Conifer', lt$taxCat)
#  All others marked as coniferous get lumped into 'Unknown or Other Deciduous'
lt$taxCat <- ifelse(is.na(lt$taxCat) & lt$TREE_TYP=='Coniferous', 'Unknown or Other Conifer', lt$taxCat)

# Broadleaf evergreen species
lt$taxCat <- ifelse(lt$SPECIES=='LIVE OAK' |
                    lt$SPECIES=='UNKNOWN BROADLEAF' |
                    (lt$SPECIES=='UNKNOWN' & lt$TREE_TYP=='Broadleaf Evergreen'), 'Unknown or Other Broadleaf Evergreen', lt$taxCat)
#  All others marked as broadleaf evergreen left get lumped into 'Unknown or Other Deciduous'
lt$taxCat <- ifelse(is.na(lt$taxCat) & lt$TREE_TYP=='Broadleaf Evergreen', 'Unknown or Other Broadleaf Evergreen', lt$taxCat)

# Snags
lt$taxCat <- ifelse(lt$SPECIES=='DEAD ASH' |
                     lt$SPECIES=='ELM (DEAD)' |
                     lt$SPECIES=='DEAD COTTONWOOD' |
                     lt$SPECIES=='SNAG' |
                     lt$SPECIES=='JUNIPER (SNAG)' |
                     lt$SPECIES=='PONDEROSA FIRE SNAG' |
                     lt$SPECIES=='SNAG COTTONWOOD', 'Snag', lt$taxCat)

return(lt)
}
