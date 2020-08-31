
import arcpy 
from arcpy import env  

print "Set local variables"

in_features = r"E:\politica_ambiental\shp\cobertura\colombia\Cobertura_tierra_2010_2012\Cobertura_tierra_2010_2012\SHP\Cobertura_tierra_2010_2012.shp" 
clip_features = r"E:\politica_ambiental\shp\base\veredas_mas_monterrey.shp" 
out_feature_class = r"E:\politica_ambiental\shp\cobertura\monterrey\all\coberturas_2010.shp" 
xy_tolerance = ""  
arcpy.Clip_analysis(in_features, clip_features, out_feature_class, xy_tolerance) 
print "Done"