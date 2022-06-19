$.widget( "custom.cwidgetbase", {
  // default options
  options: {
	  config:null,
	  onActive:function(){
		  
	  },
	  onClose:function(){
		  
	  }
  },
  geoStructure:{
	  tree:[],
	  ids:[]
  },
  currentData:null,
  themeCreated:false,
  hasChanged:false,
  showStratsMessage:false,
  firstBoot:true,
  // the constructor
  _create: function() {
	var obj = this;
	  
	obj.id = obj.element.attr('id');
	  
	//load Data configuration   Proceso de carga inicial ---------------------------------------------------------------------------------------------------
	obj.loadConfiguration(function(geoLevels){
			obj.processGeoLevels(geoLevels.data.data);
			console.log(geoLevels.data.data);
			//changeDefault geo  
			var pos = MDM6('getMyLocation');

			//obj.options.config.startingData.geoSelected = ['00'];//[pos.cityCode];

			obj.element.attr('cantheme',false); //deshabilita paneles si viene en 00
			setTimeout(function(){  //abre la configuración
				obj.openConfig();
				setTimeout(function(){
					$("#cwidgetbase_geo_content").Popup({content:'Seleccione una entidad para comenzar',showOn:'now',highlight :false,time:6000});
				},1500);
			},1500);

			obj.currentData = $.extend({},true,obj.options.config.startingData);
			obj.settings = $.extend({},true,obj.options.config.settings);

			obj.element
			  // add a class for theming
			  .addClass( "custom-cwidgetbase toolCustomIdentify cwidgetbase-animated" ).attr('collapsed','true').attr('seltab','geo').attr('changed','false').attr('geotype',obj.currentData.geoType)
			  .attr('infopanel','data')
			  // prevent double click to select text
			  .disableSelection();
			obj.id = obj.element.attr('id');

			obj.element.addClass('no-print');

			obj.options.onStart();  
			obj.createUI();

			//obj.gotoMyLocation();

			obj.options.onActive();  

			obj._refresh();

			var url = obj.options.config.settings.bootDialog;
			obj.openDialog(url,obj.options.config.settings.bootDialogTitle);
		
	}); //fin de proceso de carga inicial -----------------------------------------------------------------------------------------------------------------
	  
  },
	
  // called when created, and later when changing option
  _refresh: function() {
	  // trigger a callback/event
	  var obj = this;
	  
	  
	  
  },

  // events bound via _on are removed automatically
  // revert other modifications here
  _destroy: function() {
	this.options.onClose();
	this.onClose();
	this.element
	  .removeClass( "custom-cwidgetbase" )
	  .enableSelection();
  },

  // _setOptions is called with a hash of all options that are changing
  // always refresh when changing options
  _setOptions: function() {
	// _super and _superApply handle keeping the right this-context
	this._superApply( arguments );
	this._refresh();
  },
  getCurrentGeoLevel:function(){
	  var obj = this;
	  var levels = obj.options.config.settings.geoLevels;
	  var level = obj.currentData.geoLevel;
	  var r = null;
	  for(var x in levels){
		  if(levels[x].level == (level+1)){
			  r = levels[x];
			  break;
		  }
	  }
	  return r;
  },
  processGeoLevels:function(geoLevels){
	  var obj = this;
	  var levels = obj.options.config.settings.geoLevels;
	  var getGeoLevel = function(level){
		  var r = null;
		  for(var x in geoLevels){
			  if(geoLevels[x].level == level){
				  r = geoLevels[x];
				  break;
			  }
		  }
		  return r;
		  debugger;
	  }
	  
	  for(var x in geoLevels){
		 var elem = geoLevels[x];
		 var sub = elem.level_theme;
		 if(sub){
			 var list = sub.split(',');
			 var tlist = [];
			 for(var y in list){
				 item = getGeoLevel(list[y]);
				 tlist.push({id:item.alias,label:item.label})
				 
			 }
			 elem.themes = tlist;
			 levels.push(elem); 
		 }
	  }
  },
  loadConfiguration:function(func){
	 	var obj = this;
		var cData = obj.currentData;

		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true,{},ds.getGeoConfig);
		 obj.getData(dataSource,{},function(data){
			 if($.isFunction(func))
				 func(data);
	 	});
  },
  // _setOption is called for each individual option that is changing
  _setOption: function( key, value ) {
	// prevent invalid color values
	this._super( key, value );
  },
  gotoMyLocation:function(){
	var obj = this;
	var pos = MDM6('getMyLocation');
	//obj.options.extent('POINT('+pos.lon+','+pos.lat+')');
	obj.gotoExtent(pos.cityCode);
  },
	
  getVarsData:function(){
	var obj = this;
	if(obj.element.attr('collapsed') == 'true'){
		return obj.currentData;
	}else{
		return obj.backupData;
	}
  },
  //------------------------
	updateRampStrat:function(){
		var obj = this;
		var cd = obj.currentData; 
		var strats = cd.strats;
		
		var cadena = '';
		cadena += '<div class="cwidgetbase-header-colors-years-label"><div id="cwidgetbase-year-min" class="cwidgetbase-header-year-label-left"></div><div id="cwidgetbase-year-max" class="cwidgetbase-header-year-label-right"></div></div>';

		var colors = obj.currentData.colors.colors;
		for (var x in colors) {
			if(parseInt(x,10) < strats){
				var color = colors[x];
				cadena += '<div class="cwidgetbase-header-colors-item" style="background-color:' + color + ';width:' + Math.floor(100 / strats) + '%"></div>';
			}
		}
		cadena += '</div>';
		
		$('#cwidgetbase_color_container').html(cadena);

	},
   createUI:function(){
	  var obj = this;
	  var cd = obj.currentData;  
	   
	  obj.printGeoList();
	  var 	cadena = '<div id="cwidgetbase_header" class="cwidgetbase-header cwidgetbase-resize cwidgetbase-transition">';
			cadena+=	'<div id="cwidgetbase_header_click_collapsed"></div>';
	   		cadena+=	'<div id="cwidgetbase_header_logoLeft" idref="" class="cwidgetbase-logo-left"></div>';
	   		cadena+=	'<div id="cwidgetbase_header_title" class="cwidgetbase-truncate-text"><div id="cwidgetbase_var_title">Seleccione una variable</div><div id="cwidgetbase_subvar_title"></div></div>';
	   		cadena+=	'<div class="cwidgetbase-header-btns">';
	   		cadena+=	'	<div id="cwidgetbase_header_btnRight" idref="" class="cwidgetbase-header-btn">';
			cadena+=	'		<div id="cwidgetbase_header_btnRight_modify" idref="" class="cwidgetbase-header-btn-inner sprite-cwidgetbase sprite-cwidgetbase-modify"></div>';
	   		cadena+=	'		<div id="cwidgetbase_header_btnRight_ok_nm" idref="" class="cwidgetbase-header-btn-inner sprite-cwidgetbase sprite-cwidgetbase-down"></div>';
			cadena+=	'		<div id="cwidgetbase_header_btnRight_ok_n" idref="" class="cwidgetbase-header-btn-inner sprite-cwidgetbase sprite-cwidgetbase-down"></div>';
	   		cadena+=	'	</div>';
	   		cadena+=	'	<div id="'+obj.id+'_header_btnRight_close" idref="" class="cwidgetbase-header-btnRight-close cwidgetbase-header-btn-inner sprite-cwidgetbase sprite-cwidgetbase-info"></div>';
	   		cadena+=	'</div>';
			cadena+=	'<div class="cwidgetbase-header-colors" id="cwidgetbase_color_container">';
	   		/*cadena+=	'<div class="cwidgetbase-header-colors-years-label"><div id="cwidgetbase-year-min" class="cwidgetbase-header-year-label-left"></div><div id="cwidgetbase-year-max" class="cwidgetbase-header-year-label-right"></div></div>';

			var colors = obj.currentData.colors.colors;
	   		for(var x in colors){
				var color = colors[x];
				cadena+=	'<div class="cwidgetbase-header-colors-item" style="background-color:'+color+';width:'+Math.floor(100/colors.length)+'%"></div>';
			}
	   
			cadena+=	'</div>';*/
	   
	  		cadena+= '</div>';
			cadena+= '<div id="cwidgetbase_panels_container" class="cwidgetbase-panels-container cwidgetbase-animated" panel="vars">';
	   		cadena+= '	<div id="cwidgetbase_container" class="cwidgetbase-container cwidgetbase-tab-container" parents="false" typeselection="'+obj.currentData.typeVarSelection+'">';

	   		cadena+= '		<div id="cwidgetbase_geo_content_options" class="cwidgetbase-geo-content-options cwidgetbase-resize cwidgetbase-animated">';
			cadena+= '			<div idref="edomun" id="cwidgetbase_tool_total" class="cwidgetbase-tool-option cwidgetbase-tool-total">TOTAL</div>';
	   		cadena+= '			<div idref="edo" id="cwidgetbase_tool_edo" class="cwidgetbase-tool-option cwidgetbase-tool-total">EDO</div>';
	   		cadena+= '			<div idref="mun" id="cwidgetbase_tool_mun" class="cwidgetbase-tool-option cwidgetbase-tool-total">MUN</div>';
			cadena+= '		</div>';
			
	   		cadena+= '		<div id="cwidgetbase_content" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-content"></div>';
	   		cadena+= '		<div id="cwidgetbase_bk_btn" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-bk-btn"><div class="cwidgetbase-bk-btn-icon sprite-cwidgetbase sprite-cwidgetbase-bback"></div></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="cwidgetbase_geo_container" class="cwidgetbase-geo-container cwidgetbase-tab-container">';
			cadena+= '		<div id="cwidgetbase_geo_bk_btn" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-geo-bk-btn"><div class="cwidgetbase-geo-bk-btn-icon sprite-cwidgetbase sprite-cwidgetbase-bback"></div></div>';
	   		cadena+= '		<div id="cwidgetbase_geo_content" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-geo-content"></div>';
	   		cadena+= '		<div id="cwidgetbase_geo_type" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-geo-type">';
			cadena+= '		</div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="cwidgetbase_conf_container" class="cwidgetbase-conf-container cwidgetbase-tab-container">';
			cadena+= '		<div id="cwidgetbase_conf_content" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-conf-content"></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="cwidgetbase_info_container" class="cwidgetbase-info-container cwidgetbase-tab-container">';
			cadena+= '		<div id="cwidgetbase_info_content" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-info-content"></div>';
	   		cadena+= '		<div id="cwidgetbase_info_graph" class="cwidgetbase-resize cwidgetbase-animated cwidgetbase-info-graph">Test</div>';
			cadena+= '	</div>';
	   		cadena+= '</div>';
	   
	   		cadena+= '<div id="cwidgetbase_toolbar_container" class="cwidgetbase-toolbar-container cwidgetbase-animated">';
	   		cadena+= '	<div id="cwidgetbase_tab_geo" idref="geo" class="cwidgetbase-resize cwidgetbase-tab"><div class="sprite-cwidgetbase-globe"></div><div>Geográfico</div></div>';
	   		cadena+= '	<div id="cwidgetbase_tab_vars" idref="var" class="cwidgetbase-resize cwidgetbase-tab"><div class="sprite-cwidgetbase-vars"></div><div>Indicador</div></div>';
	   		cadena+= '	<div id="cwidgetbase_tab_graph" idref="graph" class="cwidgetbase-resize cwidgetbase-tab"><div class="sprite-cwidgetbase-graph"></div><div>Estratos</div></div>';
	   		cadena+= '	<div id="cwidgetbase_tab_info" idref="info" class="cwidgetbase-resize cwidgetbase-tab"><div class="sprite-cwidgetbase-binfo"></div><div>Detalle</div></div>';
	   		cadena+= '	<div id="cwidgetbase_tab_bottom_container" idref="info" class="cwidgetbase-tab-bottom-container cwidgetbase-animated">';
			cadena+= '		<a href="'+obj.settings.mainDoc+'" target="_blank"><div id="cwidgetbase_tab_pdfdoc" idref="pdfdoc" class="cwidgetbase-resize sprite-cwidgetbase-doc-pdf"></div></a>';
			cadena+= '	</div>';
	   		cadena+= '	<div class="cwidgetbase-disable-toolbars"></div>';
			cadena+= '</div>';

			
	  obj.element.html(cadena);
	   
	  //obj.updateRampStrat(); 
	  	
	   $('.cwidgetbase-tool-option').each(function(){
		  $(this).click(function(){
			 var idref = $(this).attr('idref');
			 
			 if(idref != obj.currentData.typeVarSelection){
				obj.hasChanged = true;	  
				$('#cwidgetbase_container').attr('typeselection',idref);
				 obj.currentData.typeVarSelection = idref;
				 obj.updateHeader();
				 
				 obj.prepareTheme();
			 }
		  });
	   });
	   $('#cwidgetbase_header_click_collapsed').click(function(){
			$('#cwidgetbase_header_btnRight').click();	   
	   });
	   
	   $('#'+obj.id+'_header_btnRight_close').click(function(e){
		   var url = obj.options.config.settings.bootDialog;
		   obj.openDialog(url,obj.options.config.settings.bootDialogTitle);
		   e.stopPropagation();
	   });
	   
	   $('.cwidgetbase-tab').each(function(){
		   $(this).click(function(e){
			   var idref = $(this).attr('idref');
			   obj.element.attr('seltab',idref);
			   e.stopPropagation();
		   })
	   });
	   
	   
	  $('#cwidgetbase_header').click(function(e){
			  if($('#cwidgetbase_panels_container').height() == 0){
				//obj.openConfig();
				e.stopPropagation();
			  }
   	 });

	  
	  $('#cwidgetbase_header_btnRight').click(function(){
		  if($('#cwidgetbase_panels_container').height() == 0){
			obj.openConfig();
		  }else{
			obj.closeConfig();
		  }
	   });
	   $('#cwidgetbase_geo_bk_btn').click(function(){
		   if(obj.currentData.geoLevel > 0){
			  obj.currentData.geoLevel--;
			  obj.currentData.geoIndex = obj.geoStructure.tree[obj.currentData.geoLevel].parent;
			  obj.printGeoList();
			  //obj.currentData.tree.pop();
			  
		   }
	   });
	   $('#cwidgetbase_bk_btn').click(function(){
		   if(obj.currentData.tree.length > 1){
			  obj.currentData.tree.pop();
			  if(obj.currentData.tree.length <= 1){
				  $('#cwidgetbase_container').attr('parent','false');
			  }
			  obj.loadTree(obj.currentData.tree[obj.currentData.tree.length-1].id,true);
		   }
	   });
	  
	  obj.element.fadeIn();
	  
  },
//print geoTypes
 printGeoTypes:function(){
	var obj = this;
	var geo = obj.currentData.geoSelected[0]; 
	var cData = obj.currentData;
	var level = obj.currentData.geoLevel;
	var geoLevels = obj.options.config.settings.geoLevels;
	debugger;
	var currentMapTheme = obj.currentData.currentMapTheme;
	if(!currentMapTheme){
		obj.currentData.currentMapTheme = geoLevels[level].themes[0].id;
		currentMapTheme = geoLevels[level].themes[0].id;
	}
	
	var canBeSelected = function(id){
		var r = false;
		if(obj.backupData && obj.backupData.geoLevel == cData.geoLevel){
			var types = geoLevels[level].themes;
			for(var x in types){
				if(types[x].id == currentMapTheme)
					r = true;
			}
		}
		return r;
	}
	 
	var list = geoLevels[level].themes;
	var cadena = '';
	 
	
	for(var x in list){
		var sItem = list[x];
		var isSelected = (currentMapTheme == sItem.id && canBeSelected())?'selected':'';

		cadena+= '<div idref="'+sItem.id+'" class="cwidgetbase-geotype-item '+isSelected+'">'+sItem.label+'</div>';
	}
	

	$('#cwidgetbase_geo_type').html(cadena);
	$('.cwidgetbase-geotype-item').each(function(){
		$(this).click(function(){
			if(canBeSelected()){
				var idref = $(this).attr('idref');
				obj.currentData.currentMapTheme = idref;
				$('.cwidgetbase-geotype-item.selected').removeClass('selected');
				$(this).addClass('selected');
				obj.updateHeader();
				obj.prepareTheme();
			}
		});
	});
	
 },
 //--------- Vars Display-----------------------------------------------------------------
  reloadTree:function(){
	  var obj = this;
	  var id = obj.currentData.tree[obj.currentData.tree.length-1].id;
	  obj.loadTree(id,true);
  },
  loadTree:function(id,reload){
	  var obj = this;
	  var cData = obj.currentData;
	  
	  var ds = obj.options.config.dataSources;
	  var dataSource = $.extend(true,{},ds.varlist);
	  var params = {
		  		id:id,
		  		level:cData.geoType,
		  }

	  if(!reload || obj.currentData.tree.length == 0){
		  obj.getData(dataSource,params,function(data){
			   //si el elemento es una recarga no agrega el elemento al arbol de peticiones
			  if(data.data && data.data.data){
				  
				   var list = data.data.data || [];
				   obj.currentData.tree.push({id:id,list:list});
				   //si hay mas de un nivel, coloca el boton de regreso
				   if(obj.currentData.tree.length > 1){
						$('#cwidgetbase_container').attr('parent','true');
				   }
				   
				   if(data && data.data && list){
					   obj.printItemList(list);
				   }
			   }
			   
		  });
	  }else{
		  obj.printItemList(obj.currentData.tree[obj.currentData.tree.length-1].list);
	  }
  },
  updateHeader:function(){
	var obj = this;
	var _var = obj.currentData.varActive;
	  
	//actualiza variable que determina si se presentarán o no los totales
	var geoType = obj.currentData.geoType;
	var typeVar = obj.currentData.typeVarSelection;
	obj.currentData.showTotal = 'mun';
	if(geoType == 'edo'){
		obj.currentData.showTotal = typeVar;
	}
	obj.element.attr('geotype',obj.currentData.geoType);
	  
	$('#cwidgetbase_var_title').html(_var.descripcion);
  
	var svars = [];
	if(obj.currentData.tree.length > 1){

	  var ttree = $.extend({},obj.currentData.tree);
	  
	  var size = function(){var c = 0; for(var x in ttree){c++;}return c}();
	  var count = 0;
	  for(var x in ttree){
		  var item = ttree[x];
		  var sel = (size-1 > parseInt(x))?ttree[parseInt(x)+1].id:null;
		  var list = item.list;
		  if(sel){
			  for(var y in list){
				  if(list[y].id == sel){
					svars.push(list[y].descripcion);  
					break;
				  }
			  }
		  }else{
			 /* if(count == 0){
				  svars.push(list[y].descripcion);  
			  }*/
		  }
		  count++;
		  
	  }
	  //svars.splice(-1,1);
	}
	 
//	var endText = 'a nivel '+obj.options.config.settings.geoTypes[parseInt(obj.currentData.geoType)-1].val;
	  
//	$('#cwidgetbase_subvar_title').html(svars.join('/')+' '+endText);
	$('#cwidgetbase_subvar_title').html(svars.join('/'));
	obj.element.attr('changed',obj.hasChanged);
	//imprime valores de tema en min max
  },
  printItemList:function(list){
	  var obj = this;
	  var cadena = '';
	  var count = 0;
	   for (var x in list){
		   var item = list[x];
		   var active = (obj.currentData.varActive && obj.currentData.varActive.id == item.id);
		   var canSelect = (item.theme);
			
			cadena+= '<div class="cwidgetbase-list-item" idref="'+item.id+'"><label>';
		   
		   if(item.metadata)
			cadena+= '	<span idref="'+item.variable+'" class="cwidgetbase-info-var-icon sprite-cwidgetbase-info"></span>';
			
		    cadena+= 	item.descripcion+'</label>';
			
			if(canSelect){
				if(!active){
					cadena+= '	<div idref="'+item.id+'" class="cwidgetbase-item-check sprite-cwidgetbase sprite-cwidgetbase-circle"></div>';   
				}else{
					cadena+= '	<div idref="'+item.id+'" class="cwidgetbase-item-check sprite-cwidgetbase sprite-cwidgetbase-ok"></div>';   
				}
			}
			
			if(item.subcat){
				cadena+= '<div idref="'+item.id+'" class="cwidgetbase-item-forward sprite-cwidgetbase sprite-cwidgetbase-forward"></div>';   
			}
			cadena+= '</div>';
	   }
	   $('#cwidgetbase_content').html(cadena);
	  
	   $('.cwidgetbase-info-var-icon').each(function(){
		  $(this).click(function(){
			  var idref = $(this).attr('idref');
			  obj.openMetadataVar(idref);
		  });
	   });
	  
	   $('.cwidgetbase-item-check').each(function(index, element) {
		   $(this).click(function(e){
				var idref = $(this).attr('idref');
				obj.selectVar(idref);
				e.stopPropagation();
		   })
		});
	   
	   $('.cwidgetbase-item-forward').each(function(index, element) {
		   $(this).click(function(e){
				var idref = $(this).attr('idref');
			   	obj.selectIndex(idref);
				e.stopPropagation();
		   })
		});
  },
  getVar:function(id){
	  var obj = this;
	  var r = null;
	  var list = obj.currentData.tree[obj.currentData.tree.length-1].list;
	  for(var x in list){
		  var item = list[x];
		  if(item.id == id){
			 r = item; 
			 break;
		  } 
	  }
	  return r;
  },
//------------------Geo display---------------------------------------	
 gotoExtent:function(cvegeo){
	var obj = this;
	var ds = obj.options.config.dataSources;
	 
	var cData = obj.currentData; 
	var alias = obj.options.config.settings.geoLevels[cData.geoLevel].alias;
	 
	var dataSource = $.extend(true,{},ds.getExtent);
	  obj.getData(dataSource,{alias:alias, cve:cvegeo},function(data){
		  if(data.response.success){
				var extent = data.data.data.extent;
			  	obj.options.extent(extent);
		  }
	  });
 },
 getGeoListItems:function(func){
	var obj = this;
	obj.getGeoList(obj.currentData.geoIndex,function(data){
		if(data && data.data){
			var list = 	data.data;
			func(list);
		}
	});
	
 },
 printGeoList:function(isRefresh){
	var obj = this;
	 
	 var printList = function(list){
		if(list.length > 0){
			var cadena = '';
			for(var x in list){
					var item = list[x];
					//borrar
					item.nombre = item.Nombre;
					var isChild = (obj.currentData.geoLevel > 0);
					$('#cwidgetbase_geo_container').attr('parent',isChild);
				
					var isSelected = (obj.currentData.geoSelected.indexOf(item.cvegeo) >= 0);
					cadena+= '<div class="cwidgetbase-geoEdo-item" label="'+item.nombre.toLowerCase()+'" idparent="'+parent+'"  idref="'+item.cvegeo+'" '+((isSelected)?'selected="selected"':'')+'>';
					cadena+= '	<div class="cwidgetbase-geoEdo-item-label">'+item.nombre+'</div>';
					cadena+= '	<div class="cwidgetbase-geoEdo-icon" idref="'+item.cvegeo+'">';
					cadena+= '		<div class="cwidgetbase-geoEdo-icon-sel sprite-cwidgetbase-circle"></div>';
					cadena+= '		<div class="cwidgetbase-geoEdo-icon-unsel sprite-cwidgetbase-ok"></div>';
					cadena+= '	</div>';
					
					//comentado VER MAS...
					if(item.childs){ //preenta icono de avanzar solo cuando llega hasta nivel de municipio
						cadena+= '	<div idref="'+item.cvegeo+'" class="cwidgetbase-geo-seemore">';
						cadena+= '		<div idref="'+item.cvegeo+'"class="cwidgetbase-geoEdo-icon-seemore sprite-cwidgetbase-forward"></div>';
						cadena+= '	</div>';
					}
					cadena+= '</div>';
			 }
			$('#cwidgetbase_geo_content').html(cadena);
			
			$('.cwidgetbase-geoEdo-icon').each(function(){
				$(this).click(function(e){
					var idref = $(this).attr('idref');
					obj.currentData.strats = 5; 
					obj.selGeoItem(idref);
					e.stopPropagation();
				})
			});
			$('.cwidgetbase-geo-seemore').each(function(index, element) {
			   $(this).click(function(e){
					var idref = $(this).attr('idref');
					obj.currentData.geoIndex = idref;
				    obj.currentData.tempGeoLevel = obj.currentData.geoLevel+1; 
				    obj.printGeoList();
					e.stopPropagation();
			   })
			});
			
		}
	}
	obj.getGeoListItems(function(list){
		printList(list);
		obj.printGeoTypes(); 
		obj.updateHeader();
	});
 },
 selGeoItem:function(idgeo){
	 var obj = this;
	 var gsel = obj.currentData.geoSelected;
	 var cData = obj.currentData;
	 
	 //reinicio de selección para limitar a 1
	 gsel = [idgeo];
	 
	 if(idgeo == '00'){
		 obj.element.attr('cantheme',false);
		 obj.clearThemeLayer();
	 }else{
		 obj.element.attr('cantheme',true);
		 obj.gotoExtent(idgeo);
	 }
	 
	 obj.hasChanged = true;
	 obj.currentData.geoSelected = gsel;
	 obj.updateHeader();
	 obj.prepareTheme();
	 
	 obj.printGeoList('refresh');
	 
 },
 getGeoList:function(parent,func){
	  var obj = this;
	  var tree = obj.geoStructure.tree;
	  var geoIds = obj.geoStructure.ids;
	 
	  var cData = obj.currentData;
	  var ds = obj.options.config.dataSources;
	  var geoLevels =  obj.options.config.settings.geoLevels;
	  if(!cData.tempGeoLevel)cData.tempGeoLevel = cData.geoLevel;
	 
	  
	  /*var func = geo.callback;*/
	  
	  var obj = this;
	  var dataSource = $.extend(true,{},ds.geolist);
	 
	  var tree = obj.geoStructure.tree;
	  var ids = geoLevels;
	 debugger;
	  var params = {
		  		id:cData.geoIndex,
		  		level:ids[cData.tempGeoLevel].level,
		  		alias:ids[cData.tempGeoLevel].alias
	  		};
	 debugger;
	  obj.getData(dataSource,params,function(data){
		  if(data.response.success){
			  	var tree = obj.geoStructure.tree;
			  	cData.geoLevel = cData.tempGeoLevel;
			  	cData.tempGeoLevel = null;
			  
			  	if((tree.length-1) > cData.geoLevel)
					tree = tree.splice(tree.length-1,1);
					
			    tree[cData.geoLevel] = {index:cData.geoLevel,parent:cData.geoIndex,list:data.data};
				if ($.isFunction(func)){
					func(data.data);
			    }
		  }
	  });
  },
spinner:function(option){
	var obj = this;
	if(option == 'show'){
		if(!$('#cwidgetbase_spinner_panel').attr('id')){
			var w = obj.element.width();
			var h = obj.element.height();
			var cadena = '<div id="cwidgetbase_spinner_panel" class="cwidgetbase-spinner-panel" count="1" style="width:'+w+'px;height:'+h+'px">';
				cadena+= '	<div class="ui-widget-overlay cwidgetbase-block-overlay"></div>';
				cadena+= '	<div class="cwidgetbase-spinner-image-container"><span class="cwidgetbase-spinner-image"></div>';
				cadena+= '<div>';

			obj.element.append(cadena);	
		}else{
			var count = parseInt($('#cwidgetbase_spinner_panel').attr('count'),10);
			$('#cwidgetbase_spinner_panel').attr('count',count+1);

		}
	}else{
		if($('#cwidgetbase_spinner_panel').attr('id')){
			var count = parseInt($('#cwidgetbase_spinner_panel').attr('count'),10);
			if(count > 1){
				$('#cwidgetbase_spinner_panel').attr('count',count-1);	
			}else{
				$('#cwidgetbase_spinner_panel').remove();
			}
		}
	}
},
// Print theme detail
printThemeDetail:function(){
	var obj = this;
	var theme = obj.currentData.theme;
	var cdata =  obj.currentData;
	var detail = theme.detail;
	var strats = theme.boundaries;
	var cadena = '';
	
	var geoType = ['Distrito','Seccion'];
		geoType = geoType[parseInt(cdata.geoType)-1];
	
	
	for(var x in strats){
		var strat = strats[x];
		var w = (100/strats.length)-1;
		cadena+='<div idstrat="'+strat.stratum+'" class="cwidgetbase-strat-item-selector '+((x=='0')?'selected':'')+'" style="width:'+w+'%">';
		cadena+='	Estrato '+strat.stratum;
		cadena+='	<span class="cwidgetbase-strat-selector-color" style="background-color:rgb('+strat.rgb.split(' ').join()+')">';
		cadena+='</div>';
	}
	
		cadena+= '<div class="cwidgetbase-strat-table-container">';
		cadena+= '	<table id="cwidgetbase_strat_table" class="cwidgetbase-strat-table" strat="e1" width="100%">';
		cadena+= '	<tr class="cwidgetbase-table-head"><td>'+geoType+'</td><td align="right">Valor</td></th></tr>';
	for(var x in detail){
		var itemx = detail[x];
		var strat = itemx.stratum;
		var stratDetail = itemx.cvegeo;
		for (var y in stratDetail){
			var itemy = stratDetail[y];
			
			cadena+= '<tr idstrat="e'+strat+'"><td>'+itemy.nombre+'</td><td align="right">'+itemy.indicador+'</td></tr>';	
		}
	}
	cadena+='</table></div>';
	
	
	cadena+='<div class="cwidgetbase-strat-table-export-container">';
	//Salto a grafica
	//cadena+='<div id="cwidgetbase_switchtograph_btn" class="cwidgetbase-switchtograph-btn sprite-cwidgetbase-graph-mini"></div>';
	
	
	var exportList = obj.settings.exportTypes;
	for(var x in exportList){
		cadena+='<div idref="'+exportList[x]+'" class="cwidgetbase-export-type-item sprite-cwidgetbase-doc-'+exportList[x]+'"></div>';
	}
	cadena+='</div>';
	
	
	$('#cwidgetbase_info_content').html(cadena);
	
	
	//area de grafica
	cadena = '<div id="cwidgetbase_info_graph_container" class="cwidgetbase-info-graph-container">Cargando...</div>'
	cadena+=' <div id="cwidgetbase_switchtodata_btn" class="cwidgetbase-switchtograph-btn sprite-cwidgetbase-grid"></div>';
	$('#cwidgetbase_info_graph').html(cadena);
	
	
	
	
	$('#cwidgetbase_switchtograph_btn').click(function(){
		$('#'+obj.id).attr('infopanel','graph');
	});
	$('#cwidgetbase_switchtodata_btn').click(function(){
		$('#'+obj.id).attr('infopanel','data');
	});
	
	
	$('.cwidgetbase-export-type-item').each(function(){
		$(this).click(function(){
			var idref = $(this).attr('idref');
			obj.exportStrats(idref);
		})
	})
	
	
	
	$('.cwidgetbase-strat-item-selector').each(function(){
		$(this).click(function(){
			var idstrat = $(this).attr('idstrat');
			$('.cwidgetbase-strat-item-selector.selected').removeClass('selected');
			$(this).addClass('selected');
			$('#cwidgetbase_strat_table').attr('strat','e'+idstrat);
		});
	});
	
	
	
	//Print min max
	obj.updateRampStrat();
	$('#cwidgetbase-year-min').html(obj.formatMoney(theme.min)); //using sugar format
	$('#cwidgetbase-year-max').html(obj.formatMoney(theme.max));
	  //cwidgetbase-year-max
	
	//obj.createGraph(); temp
	
	
},
//print panel config
  createRampColor:function(ramp){
	  	var obj = this;
		var colorRamps = obj.settings.colorRamps;
		var currentRamp = obj.currentData.colors;
		var colors = ramp.colors;
		var snum = obj.settings.numStrats;
		var cadena= '<div idref="'+ramp.id+'" class="cwidgetbase-strats-colorRamp" '+((currentRamp.id == ramp.id)?'selected="selected"':'')+'>';
			for(var x in colors){
				var width = 100/snum;
				cadena+='<div class="cwidgetbase-strats-ramp-color" style="background-color:'+colors[x]+';width:'+width+'%"></div>';	
			}
			cadena+='</div>';
		return cadena; 
 },
 printConfig:function(){
	 var obj = this;
	 var cd = obj.currentData; 
	 
	 var colorRamps = obj.settings.colorRamps;
	 
	 var cadena = '';
	 if(cd.theme){	//si hay tema para desplegar
			 cadena+= '<div id="'+obj.id+'_graph_strat'+'" class="cwidgetbase-graph-strat"></div>'; 
			 cadena+= '<div class="cwidgetbase-strats-transparency-container">';
			 cadena+= '		<div class="cwidgetbase-strats-transparency-title">Transparencia</div>';
			 cadena+= '		<div id="cwidgetbase_strats_trasparencyControl" class="cwidgetbase-strats-transparency-tool"></div>';
			 cadena+= '</div>';

			 cadena+= '<div class="cwidgetbase-theme-normalInfo cwidgetbase-theme-info">';
			 cadena+= '		<div><b>Total:</b><label>'+obj.formatMoney(cd.theme.indicator)+'<label></div>';
			 cadena+= '		<div><b>Elementos:</b><label>'+obj.formatMoney(cd.theme.n)+'</label></div>';
			 cadena+= '		<div><b>D.Estd:</b><label>'+obj.formatMoney(cd.theme.sd)+'<label></div>';
			 cadena+= '</div>';

			 cadena+= '<div class="cwidgetbase-theme-mainInfo cwidgetbase-theme-info">';
			 cadena+= '		<div><b>Media:</b><label>'+obj.formatMoney(cd.theme.mean)+'<label></div>';
			 cadena+= '		<div><b>Mediana:</b><label>'+obj.formatMoney(cd.theme.median)+'<label></div>';
			 cadena+= '		<div><b>Moda:</b><label>'+obj.formatMoney(cd.theme.mode)+'<label></div>';
			 cadena+= '</div>';


			 cadena+=	'<div class="cwidgetbase-strat-method cwidgetbase-animated">';

			var methods = obj.settings.methods; //print methods
		 	 var widthMethod = Math.floor(100/(methods.length))-2;
			 for(var x in methods){
				var method = methods[x];
				cadena+=	'<div style="width:'+widthMethod+'%" val="'+method.name+'" class="cwidgetbase-strat-item '+((method.name == cd.method)?'selected':'')+'">'+method.title+'</div>';
			 }
			 cadena+=	'</div>';

		   var minStrats = obj.settings.minStrats;
		   var maxStrats = obj.settings.maxStrats;

			cadena+= '	<div class="cwidgetbase-strats-strat-title">No.Estratos</div>';
			cadena+= '	<div id="cwidgetbase_strats_data_config_strats" class="cwidgetbase-strats-data-config-strats">';
					   for(var x = minStrats; x <= maxStrats;x++){
							cadena+= '<div idref="'+x+'" class="cwidgetbase-strats-strat-item '+((x == cd.strats)?'selected':'')+'">'+x+'</div>';
					   }
			cadena+= '	</div>';


		/*	 cadena+=	'<div class="cwidgetbase-years cwidgetbase-animated">';
			 cadena+=	'	<div class="cwidgetbase-years-title">Año</div>';

			 var years = obj.settings.years;  
			 for(var x in years){
				var year = years[x];
				cadena+=	'<div val="'+year+'" class="cwidgetbase-years-item '+((year == cd.year)?'selected':'')+'">'+year+'</div>';
			 } 
			 cadena+=	'</div>';*/


			//cadena+=	'<div class="cwidgetbase-config-titles"><label></label></div>'; 
			cadena+=	'<div class="cwidgetbase-strats-currentRamps-container">';

			 for(var x in colorRamps){
					var ramp = colorRamps[x];   
					cadena+= obj.createRampColor(ramp); 
			 }
			 cadena+=	'</div>';
	 }
	 
	 $('#cwidgetbase_conf_container').html(cadena);
	 
	$('.cwidgetbase-strats-strat-item ').each(function(index, element) {
		$(this).click(function(e){
			$('.cwidgetbase-strats-strat-item.selected').removeClass('selected');
			$(this).addClass('selected');
			obj.showStratsMessage = true;
			obj.currentData.strats = parseInt($(this).attr('idref'),10);
			obj.hasChanged = true;
			obj.updateHeader();
			
			obj.prepareTheme();
		});
	});
	 
	 
	 $( "#cwidgetbase_strats_trasparencyControl" ).slider({
		  range: "max",
		  min: 1,
		  max: 100,
		  value: obj.settings.transparency,
		  slide: function( event, ui ) {
			obj.settings.transparency = ui.value;
			obj.settings.transparency = ui.value;
			obj.options.onTransparency(ui.value);
		  }
	 });
	 
	 $('.cwidgetbase-strats-colorRamp').each(function(){
			$(this).click(function(){
				
				$('.cwidgetbase-strats-colorRamp[selected=selected]').each(function(index, element) {
						$(this).removeAttr('selected');
				});
				$(this).attr('selected','selected');
				var idref = $(this).attr('idref');
	   			
				obj.rollbackColor = $.extend(true,{},obj.currentData.colors);
				obj.currentData.colors = obj.settings.colorRamps[parseInt(idref,10)];
				obj.changeColorMap();
				obj.hasChanged = true;
				obj.updateHeader();
				obj.printGraphData();
				
				obj.prepareTheme();
				//obj.printThemeStats();
			})   
	   })
	 
	 
	 
	 $('.cwidgetbase-strat-item').each(function(index, element) {
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.cwidgetbase-strat-item.selected').removeClass('selected');
				$(this).addClass('selected');
				
				obj.currentData.method = val;
				obj.hasChanged = true;
				obj.updateHeader();
				
				obj.prepareTheme();
				
		    });  
      });
	 /* $('.cwidgetbase-years-item').each(function(index, element) { 
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.cwidgetbase-years-item.selected').removeClass('selected');
				$(this).addClass('selected');
				
				cd.year = parseInt(val,10);
				obj.hasChanged = true;
				obj.updateHeader();
				
				obj.prepareTheme();
				
		    });  
      });*/
	 
	 //print Graph
	 obj.printGraphData();
 },
 changeColorMap:function(){
	  var obj = this;
	  var data = obj.currentData;
	  var theme = data.theme;
	  var strats = theme.boundaries;
	  var colors = data.colors.colors; 
	  var dataSource = $.extend(true,{},obj.options.config.dataSources.themeColor);

	  var params = { 
					"id": theme.id, 
					 "variable": data.varActive.variable, 
				     "boundaries":[]
				    }
	  for(var x in strats){
		    var item = $.extend(true,{},strats[x]);
			var rgb = obj.hexToRgb(colors[x]);
			item.rgb = rgb.r+' '+rgb.g+' '+rgb.b
			params.boundaries.push(item);  
	  }
		obj.getData(dataSource,params,function(data){
			if(data.response.success){
				obj.options.refreshMap();	
			}
		});
  },
//---------------------------------------------------------------------	
  getData:function(source,params,callback,error,before,complete){
		var obj = this;
		if(source){
			
			var spinner = this.spinner;
			//Anexo de parametros que vengan definidos desde fuente de datos
			var s_params = source.params;
			var stringify = source.stringify;

			if (!(s_params === undefined)){
				for (var x in s_params){ //anexo de la conifuracion del origen de datos
					params[x] = s_params[x];
				};
			}
			if (!(stringify === undefined) && stringify){
				params = JSON.stringify(params);
			}
			//Estructura basica de peticion
			var dataObject = {
				   data: params,
				   success:function(json,estatus){callback(json,estatus);},
				   beforeSend: function(solicitudAJAX) {
						obj.spinner('show');
						if ($.isFunction(before)){
							before(params);
						};
				   },
				   error: function(solicitudAJAX,errorDescripcion,errorExcepcion) {
						if ($.isFunction(error)){
							error(errorDescripcion,errorExcepcion);
						};
				   },
				   complete: function(solicitudAJAX,estatus) {
						obj.spinner('hide');
						if ($.isFunction(complete)){
							complete(solicitudAJAX,estatus);
						};
				   }
			};
			//anexo de la conifuracion del origen de datos
			for (var x in source){ 
				if ( !(/field|name|id|params|stringify/.test(x)))dataObject[x] = source[x];
			};
			jQuery.support.cors = true;
			$.ajax(dataObject);
		}
	},
	prepareTheme:function(){
		var obj = this;
		var cData = obj.currentData;
		var cvar = cData.varActive.variable;
		var geoLevels = obj.options.config.settings.geoLevels;
		//tema actual;
		var currentMapTheme = obj.currentData.currentMapTheme;
		if(!currentMapTheme || obj.backupData.geoLevel != obj.currentData.geoLevel){
			obj.currentData.currentMapTheme = geoLevels[obj.currentData.geoLevel].themes[0].id;
			currentMapTheme = geoLevels[obj.currentData.geoLevel].themes[0].id;
		}
		
		
		
		if(cData.geoSelected != '00'){  //se tematiza solo cuando no es 00
			
			if(!cData.geoSelected)cData.geoSelected = ['00'];

			var dataSource = $.extend(true,{},obj.options.config.dataSources.theme);
			var currentLevelData = obj.getCurrentGeoLevel();
			
			var params = {
					ent:cData.geoSelected.join(),
					variable:cvar,
					estratos:cData.strats,
					level:currentLevelData.level,
					tipoConsulta:cData.method,
					field:currentLevelData.field_where,
					alias:cData.currentMapTheme

				}	
			var themeParams = null;
			
			obj.getData(dataSource,params,function(data){
					obj.hasChanged = false;
					obj.updateHeader();

					if(data.response.success){
						obj.backupData = $.extend({},obj.currentData);
						var vals = data.data;
						var idTheme = data.data.id;

						var themeParams = {'LAYERS':'dn1,dn2'}
						
						var levels = obj.options.config.settings.geoLevels;
						for(var x in levels){
							var alist = levels[x].themes;
							for(var y in alist){
								var alias = alist[y].id;
								if(alias == cData.currentMapTheme){
									themeParams[alias] = idTheme;
								}else{
									themeParams[alias] = 0;
								}
							}
						}
						//themeParams[cData.currentMapTheme] = idTheme; //temp
						obj.options.refreshMap(themeParams);
						
						
						
						obj.currentData.theme = vals;
						obj.printThemeDetail();

						obj.checkThemeColor();
						if(obj.stratChanged && obj.showStratsMessage){
							obj.options.systemMessage('Se modifico la configuración a '+obj.currentData.strats+' estratos',{width:240,height:120,title:'Información'});
						}
						obj.showStratsMessage = false;


						obj.stratChanged = false;
						obj.printGraphData();
						obj.printConfig();
						obj.printGeoTypes();
						obj.themeCreated = true;
						//obj.gotoExtent(cData.geoSelected.join())
					}else{
						if(!data.response.success && data.response.message == '409' && obj.currentData.strats > 1){ //409 intentar con menos estratos
							obj.currentData.strats--;
							obj.prepareTheme();
							obj.stratChanged = true;
						}else{
							obj.currentData =  obj.backupData;
							obj.backupData = null;
							obj.options.systemMessage('No se puede tematizar la variable: '+cData.varActive.descripcion,{width:240,height:120,title:'Información'});
							obj.updateHeader();
							obj.printGeoList();
							obj.loadTree(obj.currentData.index,true);

						}
						//obj.options.refreshMap(themeParams);
					}
				},function(){ //error
					if(obj.backupData != null){
						obj.currentData =  obj.backupData;
						obj.backupData = null;
						obj.updateHeader();
						obj.printGeoList();
						obj.loadTree(obj.currentData.index,true);

						obj.options.systemMessage('Error al tematizar con los valores seleccionados',{width:240,height:120,title:'Información'});
					}else{
						obj.options.systemMessage('Error al tematizar',{width:240,height:120,title:'Información'});	
					}
				});
		}
	},
	clearThemeLayer:function(){
		var obj = this;
		var themeParams = {'LAYERS':''}
		themeParams['MAPAESTATAL'] = 0;//(cData.geoType == 'edo')?idTheme:0;
		themeParams['MAPAMUNICIPAL'] = 0;//(cData.geoType == 'mun')?idTheme:0;;
		themeParams['MAPALOCALIDAD'] = 0;
		themeParams['MAPAAGEB'] = 0;
		themeParams['MAPADISTRITO'] = 0; //temp
		themeParams['MAPASECCION'] = 0;

		obj.options.refreshMap(themeParams);
	},
	onClose:function(){ //when destroy object
		var obj = this;
		var themeParams = {'LAYERS':'d100,d101,d102,d109'}
			themeParams['MAPAESTATAL'] = 0;
			themeParams['MAPAMUNICIPAL'] =0;
			themeParams['MAPALOCALIDAD'] = 0;
			themeParams['MAPAAGEB'] = 0;
		obj.options.refreshMap(themeParams);
	},
	checkThemeColor:function(){
		var obj = this;
		var cData = obj.currentData;
		if(cData.colors.id != 0){
			obj.changeColorMap();
		}
	},
  // Export
	exportStrats:function(type){
	  var obj = this;
	  var data = obj.currentData;
	  var theme = data.theme;
	  if(theme){
		  var _var = data.varActive.descripcion;  
		  var detail = theme.detail;
		  
		  var columns = ['Estrato','Clave Geográfica','Nombre','Valor'];
		  var values = [];
		  
		  for (var x in detail){
				var item = detail[x];  
				var geos = item.cvegeo;
				for(var y in geos){
					var geo_item = geos[y];
					values.push([item.stratum,geo_item.cvegeo,geo_item.nombre,geo_item.indicador]);
				}
		  }
		  //Agregar fuente y variable tematizada
		  var sourceCredits = {};
		  
		  var typeVar = obj.backupData.typeVarSelection;
		  var postList = {
					'edomun':'estatales, municipales y delegacionales',
					'edo':'estatales',
					'mun':'municipales y delegacionales',
		  }

		  _var+= ' '+postList[typeVar];
		  
		  values.push([]);
		  values.push(['Variable:'+_var])
		  values.push(['Fuente:'+obj.settings.sourceDataInfo]);
		  
		  var ds = $.extend({},true,obj.options.config.dataSources.exportData);
		  var params = {title:'Detalle de Estratos',columns:columns,values:values};
		  obj.getData(ds,params,function(data){
			  if(data && data.response.success){
			  	var url = ds.urlGet+'/'+type+'/'+data.data.id;
			  	window.location.assign(url);
			  	console.log(url);
			  }
		  });
		  //obj.getData();
	  }
  },
	
	openMetadataVar:function(_var){
		var obj = this;
		var path = obj.settings.docPath;
		var geoType = obj.currentData.geoType;
		var doc = path+'/'+_var+'.html';
		var file='<iframe src="'+doc+'" width="100%" height="370px" class="cwidgetbase-metadata-frame"></iframe>';
		 
		obj.options.systemMessage(file,{width:600,height:420,title:'Información',buttons:{}});
	},
  //Funciones para la tematización
	selectVar:function(id){
		var obj = this;
		obj.currentData.varActive = obj.getVar(id);
		obj.hasChanged = true;
		obj.updateHeader();
		obj.reloadTree();
		
		obj.prepareTheme();
	},
	selectIndex:function(id){
		var obj = this;
		obj.currentData.index = id;
		obj.loadTree(id);   
	},
	openConfig:function(){
		var obj = this;
		obj.backupData = $.extend({},obj.currentData);
		obj.element.attr('collapsed','false');
		setTimeout(function(){
			obj.options.detectCollision(obj.element);
		},1200);
		//carga arbol en posición
		obj.loadTree(obj.currentData.index,true);
		obj.printConfig();
		obj.updateHeader();
	},
	cancelModify:function(){
		var obj = this;
		obj.hasChanged = false;
		obj.currentData = obj.backupData;
		obj.reloadTree();
		obj.printThemeDetail();
		obj.printGeoList(true); //just refresh
		obj.backupData = null;
		obj.closeConfig();
	},
	closeConfig:function(){
		var obj = this;
		obj.element.attr('collapsed','true');
		//obj.prepareTheme(); deshabilitado para el procesamiento del tema inmediato
	},
	printGraphData:function(){
		var obj = this;
		
		var data = obj.currentData;
		var colors = data.colors.colors;
		var theme = data.theme;
		if(theme){
		  var detail = theme.detail;
		  var title = data.varActive.descripcion;  
		  var cadena = '';	
		 //var graphObj = obj.crateGraphData(detail,title);	 //grafica anterior, bloquedas por el momento Marzo 2017
		  for (var x in detail){
			  var item = detail[x];
			   var color = colors[x]
			  cadena+= '<div class="cwidgetbase-stratum-detail-item">';
			  cadena+= '	  	<div class="cwidgetbase-stratum-detail-item-color" style="background-color:'+color+';"></div>';
			  cadena+= '	  	<div class="cwidgetbase-stratum-detail-item-info">';
			  cadena+= '			<div class="cwidgetbase-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="cwidgetbase-stratum-detail-item-info-label">Estrato</label>';
			  cadena+= '				<label class="cwidgetbase-stratum-detail-item-info-value">'+item.stratum+'</label>';
			  cadena+= '			</div>';
			  cadena+= '			<div class="cwidgetbase-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="cwidgetbase-stratum-detail-item-info-label">Frecuencia</label>';
			  cadena+= '				<label class="cwidgetbase-stratum-detail-item-info-value">'+(item.cvegeo.length)+'</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
		  }
		  //elementos extra
		      cadena+= '<div class="cwidgetbase-stratum-detail-item">';
			  cadena+= '	  	<div class="cwidgetbase-stratum-detail-item-color cwidgetbase-confiden-bg"></div>';
			  cadena+= '	  	<div class="cwidgetbase-stratum-detail-item-info">';
			  cadena+= '			<div class="cwidgetbase-stratum-detail-item-info-vals special">';
			  cadena+= '				<label class="cwidgetbase-stratum-detail-item-info-label">Confidencialidad</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
			  cadena+= '<div class="cwidgetbase-stratum-detail-item">';
			  cadena+= '	  	<div class="cwidgetbase-stratum-detail-item-color" style="background-color:#FFF;border:1px solid #adadad"></div>';
			  cadena+= '	  	<div class="cwidgetbase-stratum-detail-item-info">';
			  cadena+= '			<div class="cwidgetbase-stratum-detail-item-info-vals special">';
			  cadena+= '				<label class="cwidgetbase-stratum-detail-item-info-label">Sin dato</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
			
			
		  $('#'+obj.id+'_graph_strat').html(cadena);
		}
	},
	crateGraphData:function(data,title){
		var obj = this;
		
		//obtieniendo colores
		var colors = obj.currentData.colors.colors;
		
		//Creando datos
		var listSerie = [{
							name:'Estratos',
							colorByPoint: true,
							data:[]
						}];
		var listDrillDown =  {
						drillUpButton: {
							relativeTo: 'spacingBox',
							position: {
								y: 0,
								x: 0
							}
						},
						series: []
					}
		
		for(var x in data){
			var sum = 0;
			var item = data[x];
			var id = item.stratum;
			
			//drillSerie
			var dserie ={
						name: 'Estrato '+id,
						id: 'estrato'+id,
						data: []
			};
			
			for(var y in item.cvegeo){
				var yitem = item.cvegeo[y];
				sum+=parseInt(yitem.indicador,10);
				dserie.data.push({
					name: yitem.cvegeo,
					label: yitem.nombre,
					y: parseInt(yitem.indicador,10),
					drilldown: 'estrato'+id
				});
			}
			listDrillDown.series.push(dserie);
			
			listSerie[0].data.push({
				name: 'E'+id,
				label: 'E'+id,
				y: sum,
				drilldown: 'estrato'+id
			})
		}
		
		var gobj = {
			chart: {
				type: 'column'
			},
			title: {
				text: ''
			},
			colors: colors,
			subtitle: {
				//text: 'Clic en las columnas para ver los detalles.'
			},
			xAxis: {
				type: 'category'
			},
			yAxis: {
				title: {
					text: ''
				}

			},
			legend: {
				enabled: false
			},
			plotOptions: {
				series: {
					borderWidth: 0,
					dataLabels: {
						enabled: false,
						format: '{point.y:.1f}%'
					}
				}
			},

			tooltip: {
				headerFormat: '<span style="font-size:11px">{series.name}</span><br>',
				pointFormat: '<span style="color:{point.color}">{point.label}</span>: <b>{point.y}</b><br/>'
			},

			series: listSerie,
			drilldown: listDrillDown
		}
		
		
		Highcharts.setOptions({
				lang:{drillUpText: "Regresar a {series.name}"},
				
		});
		
		Highcharts.chart(obj.id+'_graph_strat', gobj);
		
	},
	formatMoney:function(nStr){
            nStr += '';
			x = nStr.split('.');
			x1 = x[0];
			x2 = x.length > 1 ? '.' + x[1] : '';
			var rgx = /(\d+)(\d{3})/;
			while (rgx.test(x1)) {
				x1 = x1.replace(rgx, '$1' + ',' + '$2');
			}
			return x1 + x2;
  },
  //COLOR FUNCTIONS
  hexToRgb:function(hex) { //#FFFFFF
	var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
	return result ? {
		r: parseInt(result[1], 16),
		g: parseInt(result[2], 16),
		b: parseInt(result[3], 16)
	} : null;
  },
  openDialog:function(url,title){
	  var obj = this; 
	  
		  if(!$('#cwidgetbase-Info-dialog').attr('id')){
			  var cadena = '<div title="'+title+'" id="cwidgetbase-Info-dialog"><div id="cwidgetbase-html-doc"></div></div>';
			  $('#panel-center').append(cadena);
			  $('#cwidgetbase-Info-dialog').dialog({
				    dialogClass: 'cwidgetbase-Info-dialogContainer',
					width:550,
					height:450,
				  	modal:true,
					close: function(event, ui)
					{
						$(this).dialog('destroy').remove();
					}
			  });
		  }else{
			  $('#cwidgetbase-Info-dialog').html('<div id="cwidgetbase-html-doc"></div>').dialog();
		  }
	  
	  	  $('#cwidgetbase-html-doc').load(url);
   },
  	
//Control de ficha *********************************************************************************************************************
	onIdentify:function(identifyData){
		var obj = this;
		
		if(obj.themeCreated){  //la identificacion solo si se ha creado un tema
		
			var pos = identifyData.pos;
			var cData = obj.currentData;
			var ds = obj.options.config.dataSources;
			var dataSource = $.extend(true,{},ds.infoPoint);
			var currentLevelData = obj.getCurrentGeoLevel();
			
			var types = [null,'distrito','seccion'];
			var params = {
					cvegeo:cData.geoSelected[0],
					level:cData.geoLevel+1,
					point:'POINT('+pos.lon+' '+pos.lat+')',
					alias:cData.currentMapTheme,
					field:currentLevelData.field_where
			  }
			  obj.getData(dataSource,params,function(data){
				  if(data && data.data && data.data.data){
					  obj.getCardData(data.data.data,function(data){
						  obj.printCardData(data);
					  });
				  }else{
					  obj.options.onIdentifyFail(identifyData.pos);
				  }
			  });
		}else{
			obj.options.onIdentifyFail(identifyData.pos);
		}
	},
    getCardData:function(geo,func){
		var obj = this;
		var cData = obj.currentData;
		
		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true,{},ds.getCardValues);
		
		var geoLevels = obj.options.config.settings.geoLevels;
		var currentLevelData = obj.getCurrentGeoLevel();
		
		var params = {
				id:geo.cvegeo,
				level:currentLevelData.level,
				alias:cData.currentMapTheme
		 }
		  obj.getData(dataSource,params,function(data){
			  if(data.data){
				  data.data.geoName = geo.Nombre;
				func(data.data);
			  }
		  });
	},
	
	printCardData:function(data){
		var obj = this;	
		var cdata =  obj.currentData;
		var dataExport = [];
		
		var geoType = ['Distrito','Seccion'];
			geoType = geoType[parseInt(cdata.geoType)-1];
		
		var dialogTitle = data.geoName;
		
		if($('#toolcwidgetbase_info').attr('id'))
			$('#toolcwidgetbase_info').remove();
		
		
		var cadena = '<div id="toolcwidgetbase_info" title="'+dialogTitle+'"><div id="toolcwidgetbase_info_content"></div></div>';
			$("#panel-center").append(cadena);
		
		$('#toolcwidgetbase_info').dialog({
			resizable:false,
			width:800,
			modal: true,
			height:550,
			buttons: {
				"Cerrar": function() {
				  $( this ).dialog( "close" );
				}
			  },
			close:function(){obj.options.onCloseIdentify()}
		});
		var resume = data.general;
		var list = data.indicators;
		
		cadena = '<div id="cwidgetbase_card_header"></div>';
		cadena+= '<div class="cwidgetbase-card-title-indicators">Indicadores</div>';
		cadena+= '<div id="cwidgetbase_card_indicators_container" class="cwidgetbase-animated">';
		cadena+= '	<div id="cwidgetbase_card_indicators_closebtn"><span class="sprite-cwidgetbase-sleft"></span><span class="sprite-cwidgetbase-sright"></span></div>';
		cadena+= '	<div id="cwidgetbase_card_indicators"></div>';
		cadena+= '</div>';
		cadena+= '<div id="cwidgetbase_card_values" class="cwidgetbase-animated"></div>';
		cadena+= '<div id="cwidgetbase_card_toolbar"></div>';
		
		$('#toolcwidgetbase_info_content').html(cadena);
		
		
		//encabezado
		cadena = '<table class="cwidgetbase-table-color">';
		cadena+= '<thead><tr><td>Dato</td><td>Valor</td></tr></thead><tbody>';
		dataExport.push(['Información general']);
		dataExport.push(['Dato','Valor']);
		
		for(var x in resume){
			var item = resume[x];
			cadena+= '<tr><td>'+item.label+'</td><td>'+item.value+'</td></tr>';
			dataExport.push([item.label,item.value]);
		}
		cadena+= '</tbody></table>';
		dataExport.push(['','']);
		$('#cwidgetbase_card_header').html(cadena);
		
		//menú lateral
		var count = 1;
		cadena = '<ul>';
		for(var x in list){
			var item = list[x];
			cadena+= '<li class="'+((x==0)?'active ':'')+'geoelectoral-card-var cwidgetbase-animated" pos="'+x+'"><div idref="'+item.field+'" class="ui-icon ui-icon-info cwidgetbase-card-varid"></div><label>'+item.label+'</label></li>';
			count++;
		}
		cadena+= '</ul>'
		$('#cwidgetbase_card_indicators').html(cadena);
		
		$('.cwidgetbase-card-varid').each(function(){
			$(this).click(function(){
				var idref = $(this).attr('idref');
			  	obj.openMetadataVar(idref);
			});
		});
		
		//Contenido desglozado
		var indOrder = obj.options.config.settings.cardIndicatorOrder;
		
		cadena = '';
		count = 1;
		dataExport.push(['Indicadores','']);
		for(var x in list){
			var item = list[x];
			cadena+= '<div id="geoelectoral_ind_'+x+'" class="cwidgetbase-card-block"><div pos="'+x+'" class="cwidgetbase-card-title">'+count+'. '+item.label+'</div>';
			dataExport.push([item.label]);
			cadena+= 	'<div class="cwidgetbase-card-blockcontent">';
			cadena+= 	'<table class="cwidgetbase-table-color"><tbody>';
			
			var head = [];
			cadena+= 	'<thead><tr>';
			var _count = 0;
			for (var y in indOrder){
				var _item = indOrder[y]; 
				cadena+= '<td '+((_count == 0)?'class="geolectoral-card-table-labelcolumn"':'')+'>'+_item.label+'</td>';
				head.push(_item.label);
				_count++;
			}
			dataExport.push(head);
			
			cadena+= 	'</tr></thead>';
			
			cadena+= 	'<tbody>';
			for(var y in item.value){  //extrae fila
				var _item = item.value[y];
				
				var content = [];
				cadena+= 	'<tr>';
				//impresion de columnas
				for(var z in indOrder){
					var zitem = indOrder[z];
					var elem = _item[zitem.id];
					var alignNumber = '';
					
					if($.isNumeric(elem)){
						alignNumber = 'style="text-align:right"';
							if((elem).isInteger()){
								elem = elem.format();
							}else{
								elem = elem.format(2);
							}
					}
					
					cadena+= '<td '+alignNumber+'>'+elem+'</td>';	
					content.push(elem);
				}
				cadena+= 	'</tr>';
				dataExport.push(content);
				
			}
			cadena+= '</tbody></table>';
			cadena+= '</div></div>';
			dataExport.push(['']);
			count++;
		}
		$('#cwidgetbase_card_values').html(cadena);
		
		
		//Exportación
		var exportList = obj.settings.exportTypes;
		cadena = '';
		for(var x in exportList){
			cadena+='<div idref="'+exportList[x]+'" class="cwidgetbase-card-export-type-item sprite-cwidgetbase-doc-'+exportList[x]+'"></div>';
		}
		//inclusión de descarga nacional
		cadena+='</div>';
		
		$('#cwidgetbase_card_toolbar').html(cadena);
		
		$('.cwidgetbase-card-export-type-item').each(function(){
			$(this).click(function(){
				var idref = $(this).attr('idref');
				obj.exportIndicators(idref,dataExport);
			});
			
		});
		
		
		
		obj.setCardEvents();	
	},
	exportIndicators:function(type,edata){
	  var obj = this;
	  var cdata = obj.currentData;

	  var ds = $.extend({},true,obj.options.config.dataSources.exportData);
	  var params = {title:'Información',columns:['',''],values:edata};
	  obj.getData(ds,params,function(data){
		  if(data && data.response.success){
			var url = ds.urlGet+'/'+type+'/'+data.data.id;
			window.location.assign(url);
		  }
	  });
	  //obj.getData();
	},
	setCardEvents:function(){
		var obj = this;
		var scrollpos = [];
		var count = 0;
		var fix = 0;
		
		var jumpScrollToVar = function(pos){
			var pos = (pos === undefined)?$('.geoelectoral-card-var.active').attr('pos'):pos;
			var top = scrollpos[parseInt(pos)];
			$('#cwidgetbase_card_values').animate({scrollTop:top}, 500, 'swing', function() { 
			});
		}
		$('.cwidgetbase-card-title').each(function(){
			var top = $(this).position().top;
			if(count == 0)
				fix = top;
			scrollpos.push(top-fix);
			count++;
		});
		
		$('#cwidgetbase_card_values').scroll(function(){
			var scroll = $('#cwidgetbase_card_values').scrollTop();
			for(var x in scrollpos){
				if((scrollpos[x]-scroll)>=0){
					if((scrollpos[x]-scroll) > $('#cwidgetbase_card_values').height())
						x--;
					
					$('.geoelectoral-card-var.active').removeClass('active');
					$('.geoelectoral-card-var[pos='+x+']').addClass('active');
					break;
				} 
			}
		});
		$('.geoelectoral-card-var').each(function(){
			$(this).unbind('click');
			$(this).click(function(){
				var pos = $(this).attr('pos');
				
				jumpScrollToVar(pos);
				
				/*
				var top = scrollpos[parseInt(pos)];
				$('#cwidgetbase_card_values').animate({scrollTop:top}, 500, 'swing', function() { 
				});
				*/
				
				//$('#cwidgetbase_card_values').scrollTop(top);
			})
		});
		
		$('#cwidgetbase_card_indicators_closebtn').unbind('click');
		$('#cwidgetbase_card_indicators_closebtn').click(function(){
			var status = $('#toolcwidgetbase_info_content').attr('status');
			if(status != 'collapsed'){
				$('#toolcwidgetbase_info_content').attr('status','collapsed');
			}else{
				$('#toolcwidgetbase_info_content').attr('status','expanded');
			}
			setTimeout(function(){
				obj.setCardEvents();
				$('.geoelectoral-card-var.active').click();
			},1200);
			
		});
		
	},
	// Grafica
	createGraph: function (options) {
		var obj = this;
		var theme = obj.currentData.theme;
		var cdata =  obj.currentData;
		var detail = theme.detail;
		var strats = theme.boundaries;
		
		var width = 710;//($(window).width() * 0.7);
		var height = 325;//500 + ((s.length) * 350);
		
		var geoType = ['Distrito','Seccion'];
			geoType = geoType[parseInt(cdata.geoType)-1];
		
		var geoTypes = obj.options.config.settings.geoTypes;
			geoTypes = geoTypes[parseInt(cdata.geoType)-1].val;
		
		var varName = cdata.varActive.descripcion;
		
		//series
		var dataSeries = [];
		for(var x in detail){
			var list = detail[x].cvegeo;
			for(var y in list){
				var item = list[y];
				dataSeries[parseInt(item.nombre,10)-1] = ({
					name:item.nombre,
					y:parseFloat(item.indicador,10)
				});
			}
		}
	
		
		Highcharts.chart('cwidgetbase_info_graph_container', {
			exporting: { enabled: false },
			chart: {
				type: 'column',
				height: height,
				width: (width - 140)
			},
			title: {
				text: ''
			},
			subtitle: {
				text: varName
			},
			xAxis: {
				type: 'category',
				title : geoTypes
			},
			yAxis: {
				min: 0,
				title: {
					text: '',
					align: 'high'
				},
				labels: {
					overflow: 'justify',
					formatter: function () {
						var ret,
							numericSymbols = ['mil', 'M', 'G', 'T', 'P', 'E'],
							i = numericSymbols.length;
						if (this.value >= 1000) {
							while (i-- && ret === undefined) {
								multi = Math.pow(1000, i + 1);
								if (this.value >= multi && numericSymbols[i] !== null) {
									ret = (this.value / multi) + numericSymbols[i];
								}
							}
						}
						return (ret ? ret : this.value);
					}
				}
			},
			legend: {
				enabled: false
			},
			labels: {
					overflow: 'justify',
			},
			plotOptions: {
				series: {
					borderWidth: 0,
					dataLabels: {
						enabled: false,
						format: '{point.y}'
					}
				}
			},

			tooltip: {
				headerFormat: '<span style="font-size:11px">{series.name}</span><br>',
				pointFormat: '<span>{point.name}</span>: <b>{point.y}</b><br/>'
			},

			series: [{
				name: geoType,
				colorByPoint: true,
				data: dataSeries,
				color: '#FF00FF'
			}]
		});
		
	}
  
});


























