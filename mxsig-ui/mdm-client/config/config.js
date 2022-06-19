requirejs.config({
    paths: {
        mapConfig:'../config/mapConfig',
		dataSource:'../config/dataSourceConfig',
        uiConfig:'../config/uiConfig',
		startupConfig:'../config/controlsConfig',
		toolsConfig:'../config/toolsConfig',
		tree:'../config/tree'
		
    }
});
define(['mapConfig','uiConfig','dataSource','startupConfig','toolsConfig','tree'],function(mapConfig,uiConfig,dataSource,startupConfig,toolsConfig,tree){
   var conf = {
        map:mapConfig
    }
    if(typeof(MapConfig)!='undefined'){
        mapConfig = $.extend(mapConfig, MapConfig);
    }
    if(typeof(sourcesConfig)!='undefined'){
        dataSource = $.extend(dataSource, sourcesConfig);
    }
	if(typeof(controlConfig)!='undefined'){
        startupConfig = $.extend(startupConfig, controlConfig);
    }
	if(typeof(mdm_toolsConfig)!='undefined'){
        toolsConfig = $.extend(toolsConfig, mdm_toolsConfig);
    }
	if(typeof(mdm_tree)!='undefined'){
        tree = $.extend(tree, mdm_tree);
    }
   return{mapConfig:mapConfig,ui:uiConfig,dataSource:dataSource,startupConfig:startupConfig,toolsConfig:toolsConfig,tree:tree}
});