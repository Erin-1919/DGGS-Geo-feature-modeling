import arcpy

## Obtain script parameter values
sortFeat  = arcpy.GetParameterAsText(0)
newFile = arcpy.GetParameterAsText(1)
newCSV = arcpy.GetParameterAsText(2)

## Dissolve all cells representing the same feature
arcpy.management.Dissolve(sortFeat, newFile, "JOIN_FID", None, "MULTI_PART", "DISSOLVE_LINES")

## Add field to store new area
arcpy.AddField_management(newFile, "NewArea", "DOUBLE")

## Calculate the geodesic area
arcpy.management.CalculateGeometryAttributes(newFile, "NewArea AREA_GEODESIC", '', "SQUARE_METERS", "GEOGCS['WGS84(DD)',DATUM['WGS84',SPHEROID['WGS84',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['degree',0.0174532925199433]]")

## Output the attribute table into csv file
arcpy.conversion.TableToTable(newFile, r"E:\UCalgary\ENGO 697 Digital Earth\dggridR_Experiment\Results\Output_csv", newCSV)