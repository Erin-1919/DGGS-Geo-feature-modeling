import arcpy

## Obtain script parameter values
sortFeat  = arcpy.GetParameterAsText(0)
newFile = arcpy.GetParameterAsText(1)
newCSV = arcpy.GetParameterAsText(2)

## Add fields including lon/lat and point order
arcpy.AddField_management(sortFeat, "longitude", "DOUBLE")
arcpy.AddField_management(sortFeat, "latitude", "DOUBLE")
arcpy.AddField_management(sortFeat, "PtOrder", "SHORT")

## Calculate lon/lat
arcpy.management.CalculateGeometryAttributes(sortFeat, "longitude POINT_X;latitude POINT_Y", '', '', "GEOGCS['WGS84(DD)',DATUM['WGS84',SPHEROID['WGS84',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['degree',0.0174532925199433]]")

## Define sort fields
sortFID = 'JOIN_FID' 
sortLAT = 'latitude' 
sortLON = 'longitude' 
idField   = 'PtOrder' 

## Define a function to populate sequential numbers
def autoIncrement():
    global rec
    pStart    = 1 
    pInterval = 1 
    if (rec == 0): 
        rec = pStart 
    else: 
        rec += pInterval 
    return rec

## Call the function
rows1 = arcpy.UpdateCursor(sortFeat, "JOIN_FID = 46 Or street_typ = 'AV' Or street_typ = 'DR'", "", "", sort_fields = sortFID + " A;" + sortLON + " A;" + sortLAT + " A")

rec=0

for row in rows1:
    row.setValue(idField, autoIncrement())
    rows1.updateRow(row)

del row, rows1

rows2 = arcpy.UpdateCursor(sortFeat, "(street_typ = 'ST' And JOIN_FID <> 46) OR (street_typ = 'TR')", "", "", sort_fields = sortFID + " A;" + sortLAT + " A;" + sortLON + " A")

rec=0

for row in rows2:
    row.setValue(idField, autoIncrement())
    rows2.updateRow(row)

del row, rows2

## Convert points to lines according to point order group by JOIN_FID
arcpy.management.PointsToLine(sortFeat, newFile, "JOIN_FID", "PtOrder", "NO_CLOSE")

## Add a field to store line length
arcpy.AddField_management(newFile, "NewLength", "DOUBLE")

## Calculate the geodesic length
arcpy.management.CalculateGeometryAttributes(newFile, "NewLength LENGTH_GEODESIC", "METERS", '', "GEOGCS['WGS84(DD)',DATUM['WGS84',SPHEROID['WGS84',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['degree',0.0174532925199433]]")

## Output the attribute table into csv file
arcpy.conversion.TableToTable(newFile, r"E:\UCalgary\ENGO 697 Digital Earth\dggridR_Experiment\Results\Output_csv", newCSV)
