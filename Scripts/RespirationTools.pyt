# -*- coding: utf-8 -*-

#Micah Taylor - Pacific Northwest National Laboratory
    # Geospatial Science, Analytics, and Geointelligence team
    # Earth System Science Division
    
    #Log_Predicted_Normalized_Respiration_Rate


import arcpy


class Toolbox:
    def __init__(self):
        """Define the toolbox (the name of the toolbox is the name of the
        .pyt file)."""
        self.label = "Toolbox"
        self.alias = "toolbox"

        # List of tool classes associated with this toolbox
        self.tools = [Tool]


class Tool:
    def __init__(self):
        """Define the tool (tool name is the name of the class)."""
        self.label = "Respiration Interpolator"
        self.description = "A tool to interpolate respiration sample point data to a continuous surface within a given area (i.e. CONUS)"

    def getParameterInfo(self):
        """Define the tool parameters."""
        params = [
            arcpy.Parameter(displayName="Folder to save layer",
                            name="workspace",
                            datatype="DEWorkspace",
                            parameterType="Required",
                            direction="Input"),
            
            arcpy.Parameter(displayName="Input Respiration Point Data",
                            name="input_respiration_points",
                            datatype="DETable",
                            parameterType="Required",
                            direction="Input"),
            
            arcpy.Parameter(displayName="Column of values to Interpolate",
                            name="interpolation_values_column",
                            datatype="GPString",
                            parameterType="Required",
                            direction="Input"),
            
            arcpy.Parameter(displayName="Boundary to contain respiration contour",
                            name="mask_boundary",
                            datatype="GPFeatureLayer",
                            parameterType="Required",
                            direction="Input"),
            
            arcpy.Parameter(displayName="Output respiration interpolated raster name",
                            name="output_raster",
                            datatype="DERasterDataset",
                            parameterType="Required",
                            direction="Output")
        ]
        return params

    def isLicensed(self):
        """Set whether the tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter. This method is called after internal validation."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        workspace = parameters[0].valueAsText
        input_respiration_points = parameters[1].valueAsText
        interpolation_values_column = parameters[2].valueAsText
        mask_boundary = parameters[3].valueAsText
        output_raster = parameters[4].valueAsText
        
        arcpy.env.workspace = workspace
        
        new_point_feature_resp = arcpy.management.XYTableToPoint(
                in_table=input_respiration_points,
                out_feature_class="data_points",
                x_field="Longitude",
                y_field="Latitude",
                z_field=None,
                coordinate_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision'
                )
        
        new_point_feature_resp_prj = arcpy.management.Project(
                in_dataset=new_point_feature_resp,
                out_dataset="data_points_Projected",
                out_coor_system='PROJCS["NAD_1983_2011_Contiguous_USA_Albers",GEOGCS["GCS_NAD_1983_2011",DATUM["D_NAD_1983_2011",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1.0]]',
                transform_method="WGS_1984_(ITRF08)_To_NAD_1983_2011",
                in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
                preserve_shape="NO_PRESERVE_SHAPE",
                max_deviation=None,
                vertical="NO_VERTICAL"
            )
        
        
        interpolated_raster_unclipped = arcpy.sa.Idw(
        in_point_features=new_point_feature_resp_prj,
        z_field=interpolation_values_column,
        
        #change the code below to interpolate to a new area unit.  
        #the value will be meters x meters, where meters is the value of 'cell_size'.
        #currently set to 8046 (5 miles).
        cell_size=8046,
        power=2,
        search_radius="VARIABLE 12",
        in_barrier_polyline_features=None
                )
        
        #uncomment the line below this if you want a full extent raster along with the raster clipped to the boundary.  It will be called 'raster_full' and saved in your workspace folder or geodatabase
        #interpolated_raster_unclipped.save("raster_full")
        
        final_masked_raster = arcpy.sa.ExtractByMask(
        in_raster=interpolated_raster_unclipped,
        in_mask_data=mask_boundary,
        extraction_area="INSIDE",
        analysis_extent= mask_boundary
            )
        
        final_masked_raster.save(output_raster)
        
        return
    
    

        
        
    def postExecute(self, parameters):
        """This method takes place after outputs are processed and
        added to the display."""
        return
