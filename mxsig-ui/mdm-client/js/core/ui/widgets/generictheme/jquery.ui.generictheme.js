$.widget( "custom.generictheme", {
  // default options
  options: {
	  config:null,
	  onActive:function(){
		  
	  },
	  onClose:function(){
		  
	  }
  },
  currentData:null,
  hasChanged:false,
  showStratsMessage:false,
  firstBoot:true,
  // the constructor
  _create: function() {
	var obj = this;
	obj.id = obj.element.attr('id');
	obj.changeClassCSS();
	  
	  
	//changeDefault geo  
	var pos = MDM6('getMyLocation');
	
	//obj.options.config.startingData.geoSelected = ['00'];//[pos.cityCode];
	if(obj.options.config.startingData.geoSelected.length == 1 && obj.options.config.startingData.geoSelected[0] == '00'){
		obj.element.attr('cantheme',false); //deshabilita paneles si viene en 00
		setTimeout(function(){  //abre la configuración
			obj.openConfig();
			setTimeout(function(){
				$("#generictheme_geo_content").Popup({content:'Seleccione una entidad para comenzar',showOn:'now',highlight :false,time:6000});
			},1500);
		},1500);
	}
	  
	obj.currentData = $.extend({},true,obj.options.config.startingData);
	obj.settings = $.extend({},true,obj.options.config.settings);
	  
	this.element
	  // add a class for theming
	  .addClass( "custom-generictheme toolCustomIdentify generictheme-animated" ).attr('collapsed','true').attr('seltab','geo').attr('changed','false').attr('geotype',obj.currentData.geoType)
	  .attr('infopanel','data')
	  // prevent double click to select text
	  .disableSelection();
	this.id = this.element.attr('id');
	 
	obj.element.addClass('no-print');
	  
	obj.options.onStart();  
	obj.createUI();
	  
	//inicia tema al iniciar
	obj.prepareTheme();
	  
	//obj.gotoMyLocation();
	  
	this.options.onActive();  
	  
	this._refresh();
	  
	var url = obj.options.config.settings.bootDialog;
	  obj.openDialog(url,obj.options.config.settings.bootDialogTitle);
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
	  .removeClass( "custom-generictheme" )
	  .enableSelection();
  },

  // _setOptions is called with a hash of all options that are changing
  // always refresh when changing options
  _setOptions: function() {
	// _super and _superApply handle keeping the right this-context
	this._superApply( arguments );
	this._refresh();
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
		cadena += '<div class="generictheme-header-colors-years-label"><div id="generictheme-year-min" class="generictheme-header-year-label-left"></div><div id="generictheme-year-max" class="generictheme-header-year-label-right"></div></div>';

		var colors = obj.currentData.colors.colors;
		for (var x in colors) {
			if(parseInt(x,10) < strats){
				var color = colors[x];
				cadena += '<div class="generictheme-header-colors-item" style="background-color:' + color + ';width:' + Math.floor(100 / strats) + '%"></div>';
			}
		}
		cadena += '</div>';
		
		$('#generictheme_color_container').html(cadena);

	},
   createUI:function(){
	  var obj = this;
	  var cd = obj.currentData;  
	   
	  obj.printGeoList();
	  var 	cadena = '<div id="generictheme_header" class="generictheme-header generictheme-resize generictheme-transition">';
			cadena+=	'<div id="generictheme_header_click_collapsed"></div>';
	   		cadena+=	'<div id="generictheme_header_logoLeft" idref="" class="generictheme-logo-left"></div>';
	   		cadena+=	'<div id="generictheme_header_title" class="generictheme-truncate-text"><div id="generictheme_var_title">Seleccione una variable</div><div id="generictheme_subvar_title"></div></div>';
	   		cadena+=	'<div class="generictheme-header-btns">';
	   		cadena+=	'	<div id="generictheme_header_btnRight" idref="" class="generictheme-header-btn">';
			cadena+=	'		<div id="generictheme_header_btnRight_modify" idref="" class="generictheme-header-btn-inner sprite-generictheme sprite-generictheme-modify"></div>';
	   		cadena+=	'		<div id="generictheme_header_btnRight_ok_nm" idref="" class="generictheme-header-btn-inner sprite-generictheme sprite-generictheme-down"></div>';
			cadena+=	'		<div id="generictheme_header_btnRight_ok_n" idref="" class="generictheme-header-btn-inner sprite-generictheme sprite-generictheme-down"></div>';
	   		cadena+=	'	</div>';
	   		cadena+=	'	<div id="'+obj.id+'_header_btnRight_close" idref="" class="generictheme-header-btnRight-close generictheme-header-btn-inner sprite-generictheme sprite-generictheme-info"></div>';
	   		cadena+=	'</div>';
			cadena+=	'<div class="generictheme-header-colors" id="generictheme_color_container">';
	   		/*cadena+=	'<div class="generictheme-header-colors-years-label"><div id="generictheme-year-min" class="generictheme-header-year-label-left"></div><div id="generictheme-year-max" class="generictheme-header-year-label-right"></div></div>';

			var colors = obj.currentData.colors.colors;
	   		for(var x in colors){
				var color = colors[x];
				cadena+=	'<div class="generictheme-header-colors-item" style="background-color:'+color+';width:'+Math.floor(100/colors.length)+'%"></div>';
			}
	   
			cadena+=	'</div>';*/
	   
	  		cadena+= '</div>';
			cadena+= '<div id="generictheme_panels_container" class="generictheme-panels-container generictheme-animated" panel="vars">';
	   		cadena+= '	<div id="generictheme_container" class="generictheme-container generictheme-tab-container" parents="false" typeselection="'+obj.currentData.typeVarSelection+'">';

	   		cadena+= '		<div id="generictheme_geo_content_options" class="generictheme-geo-content-options generictheme-resize generictheme-animated">';
			cadena+= '			<div idref="edomun" id="generictheme_tool_total" class="generictheme-tool-option generictheme-tool-total">TOTAL</div>';
	   		cadena+= '			<div idref="edo" id="generictheme_tool_edo" class="generictheme-tool-option generictheme-tool-total">EDO</div>';
	   		cadena+= '			<div idref="mun" id="generictheme_tool_mun" class="generictheme-tool-option generictheme-tool-total">MUN</div>';
			cadena+= '		</div>';
			
	   		cadena+= '		<div id="generictheme_content" class="generictheme-resize generictheme-animated generictheme-content"></div>';
	   		cadena+= '		<div id="generictheme_bk_btn" class="generictheme-resize generictheme-animated generictheme-bk-btn"><div class="generictheme-bk-btn-icon sprite-generictheme sprite-generictheme-bback"></div></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="generictheme_geo_container" class="generictheme-geo-container generictheme-tab-container">';
			cadena+= '		<div id="generictheme_geo_bk_btn" class="generictheme-resize generictheme-animated generictheme-geo-bk-btn"><div class="generictheme-geo-bk-btn-icon sprite-generictheme sprite-generictheme-bback"></div></div>';
	   		cadena+= '		<div id="generictheme_geo_content" class="generictheme-resize generictheme-animated generictheme-geo-content"></div>';
	   		cadena+= '		<div id="generictheme_geo_type" class="generictheme-resize generictheme-animated generictheme-geo-type">';
			cadena+= '		</div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="generictheme_conf_container" class="generictheme-conf-container generictheme-tab-container">';
			cadena+= '		<div id="generictheme_conf_content" class="generictheme-resize generictheme-animated generictheme-conf-content"></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="generictheme_info_container" class="generictheme-info-container generictheme-tab-container">';
			cadena+= '		<div id="generictheme_info_content" class="generictheme-resize generictheme-animated generictheme-info-content"></div>';
	   		cadena+= '		<div id="generictheme_info_graph" class="generictheme-resize generictheme-animated generictheme-info-graph">Test</div>';
			cadena+= '	</div>';
	   		cadena+= '</div>';
	   
	   		cadena+= '<div id="generictheme_toolbar_container" class="generictheme-toolbar-container generictheme-animated">';
	   		cadena+= '	<div id="generictheme_tab_geo" idref="geo" class="generictheme-resize generictheme-tab"><div class="sprite-generictheme-globe"></div><div>Geográfico</div></div>';
	   		cadena+= '	<div id="generictheme_tab_vars" idref="var" class="generictheme-resize generictheme-tab"><div class="sprite-generictheme-vars"></div><div>Indicador</div></div>';
	   		cadena+= '	<div id="generictheme_tab_graph" idref="graph" class="generictheme-resize generictheme-tab"><div class="sprite-generictheme-graph"></div><div>Estratos</div></div>';
	   		cadena+= '	<div id="generictheme_tab_info" idref="info" class="generictheme-resize generictheme-tab"><div class="sprite-generictheme-binfo"></div><div>Detalle</div></div>';
	   		cadena+= '	<div id="generictheme_tab_bottom_container" idref="info" class="generictheme-tab-bottom-container generictheme-animated">';
			cadena+= '		<a href="'+obj.settings.mainDoc+'" target="_blank"><div id="generictheme_tab_pdfdoc" idref="pdfdoc" class="generictheme-resize sprite-generictheme-doc-pdf"></div></a>';
			cadena+= '	</div>';
	   		cadena+= '	<div class="generictheme-disable-toolbars"></div>';
			cadena+= '</div>';

			
	  obj.element.html(cadena);
	   
	  //obj.updateRampStrat(); 
	  	
	   $('.generictheme-tool-option').each(function(){
		  $(this).click(function(){
			 var idref = $(this).attr('idref');
			 
			 if(idref != obj.currentData.typeVarSelection){
				obj.hasChanged = true;	  
				$('#generictheme_container').attr('typeselection',idref);
				 obj.currentData.typeVarSelection = idref;
				 obj.updateHeader();
				 
				 obj.prepareTheme();
			 }
		  });
	   });
	   $('#generictheme_header_click_collapsed').click(function(){
			$('#generictheme_header_btnRight').click();	   
	   });
	   
	   $('#'+obj.id+'_header_btnRight_close').click(function(e){
		   var url = obj.options.config.settings.bootDialog;
		   obj.openDialog(url,obj.options.config.settings.bootDialogTitle);
		   e.stopPropagation();
	   });
	   
	   $('.generictheme-tab').each(function(){
		   $(this).click(function(e){
			   var idref = $(this).attr('idref');
			   obj.element.attr('seltab',idref);
			   e.stopPropagation();
		   })
	   });
	   
	   
	  $('#generictheme_header').click(function(e){
			  if($('#generictheme_panels_container').height() == 0){
				//obj.openConfig();
				e.stopPropagation();
			  }
   	 });

	  
	  $('#generictheme_header_btnRight').click(function(){
		  if($('#generictheme_panels_container').height() == 0){
			obj.openConfig();
		  }else{
			obj.closeConfig();
		  }
	   });
	   $('#generictheme_bk_btn').click(function(){
		   if(obj.currentData.tree.length > 1){
			  obj.currentData.tree.pop();
			  if(obj.currentData.tree.length <= 1){
				  $('#generictheme_container').attr('parent','false');
			  }
			  obj.loadTree(obj.currentData.tree[obj.currentData.tree.length-1].id,true);
		   }
	   });
	  
	  obj.element.fadeIn();
	  
  },
//print geoTypes
 printGeoTypes:function(geo){
	var obj = this;
	geo = geo[0]; 
	var cData = obj.currentData;
	var list = cData.geoTypes;
	
	var cadena = '';
	for(var x in list){
		var sItem = list[x];
		var isSelected = (cData.geoType == sItem.id)?'selected':'';
		cadena+= '<div idref="'+sItem.id+'" class="generictheme-geotype-item '+isSelected+'">'+sItem.val+'</div>';
	}

	$('#generictheme_geo_type').html(cadena);
	$('.generictheme-geotype-item').each(function(){
		$(this).click(function(){
			var idref = $(this).attr('idref');
			cData.geoType = idref;
			$('.generictheme-geotype-item.selected').removeClass('selected');
			$(this).addClass('selected');
			obj.hasChanged = true;
			obj.updateHeader();

			obj.prepareTheme();
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
			  if(data.data){
				   var list = data.data.registros || [];
				   
				   obj.currentData.tree.push({id:id,list:list});
				   //si hay mas de un nivel, coloca el boton de regreso
				   if(obj.currentData.tree.length > 1){
						$('#generictheme_container').attr('parent','true');
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
	  
	$('#generictheme_var_title').html(_var.descripcion);
  
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
	 
	var endText = 'a nivel '+obj.options.config.settings.geoTypes[parseInt(obj.currentData.geoType)-1].val;
	  
	$('#generictheme_subvar_title').html(svars.join('/')+' '+endText);
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
			
			cadena+= '<div class="generictheme-list-item" idref="'+item.id+'"><label>';
		   
		   if(item.metadata)
			cadena+= '	<span idref="'+item.variable+'" class="generictheme-info-var-icon sprite-generictheme-info"></span>';
			
		    cadena+= 	item.descripcion+'</label>';
			
			if(canSelect){
				if(!active){
					cadena+= '	<div idref="'+item.id+'" class="generictheme-item-check sprite-generictheme sprite-generictheme-circle"></div>';   
				}else{
					cadena+= '	<div idref="'+item.id+'" class="generictheme-item-check sprite-generictheme sprite-generictheme-ok"></div>';   
				}
			}
			
			if(item.subcat){
				cadena+= '<div idref="'+item.id+'" class="generictheme-item-forward sprite-generictheme sprite-generictheme-forward"></div>';   
			}
			cadena+= '</div>';
	   }
	   $('#generictheme_content').html(cadena);
	  
	   $('.generictheme-info-var-icon').each(function(){
		  $(this).click(function(){
			  var idref = $(this).attr('idref');
			  obj.openMetadataVar(idref);
		  });
	   });
	  
	   $('.generictheme-item-check').each(function(index, element) {
		   $(this).click(function(e){
				var idref = $(this).attr('idref');
				obj.selectVar(idref);
				e.stopPropagation();
		   })
		});
	   
	   $('.sprite-generictheme-forward').each(function(index, element) {
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
	 
	var dataSource = $.extend(true,{},ds.getExtent);
	 dataSource.url+='/'+cvegeo;
	  obj.getData(dataSource,{},function(data){
		  if(data.response.success){
				var extent = data.data.extent;
			  	obj.options.extent(extent);
		  }
	  });
 },
 getGeoListItems:function(func){
	var obj = this;
	
	var typeList = ['Estados'];
	// control Tree
	if(obj.currentData.geoTree.length == 0){
		obj.currentData.geoTree.push([{childs:false,cvegeo:"00",nombre:"Nacional"}]); //in case first geo element
		obj.currentData.geoSelected = ['00'];
	}
	 
	obj.getGeoList(obj.currentData.geoIndex,function(data){
		var list = 	(data.Estados)?data.Estados:
					(data.municipios)?data.estados:null;
		
		/*if(data.Estados){
			list.unshift({childs:false,cvegeo:"00",nombre:"Nacional"});
		}*/
		
		func(list);
		
	});
	
 },
 printGeoList:function(isRefresh){
	var obj = this;
	var typeList = ['Estados'];
	
	 var printList = function(list){
		if(list.length > 0){
			var cadena = '';
			for(var x in list){
					var item = list[x];
					var isSelected = (obj.currentData.geoSelected.indexOf(item.cvegeo) >= 0);
					cadena+= '<div class="generictheme-geoEdo-item" label="'+item.nombre.toLowerCase()+'" idparent="'+parent+'" idref="'+item.cvegeo+'" '+((isSelected)?'selected="selected"':'')+'>';
					cadena+= '	<div class="generictheme-geoEdo-item-label">'+item.nombre+'</div>';
					cadena+= '	<div class="generictheme-geoEdo-icon" idref="'+item.cvegeo+'">';
					cadena+= '		<div class="generictheme-geoEdo-icon-sel sprite-generictheme-circle"></div>';
					cadena+= '		<div class="generictheme-geoEdo-icon-unsel sprite-generictheme-ok"></div>';
					cadena+= '	</div>';
					
					//comentado VER MAS...
					if(item.childs && false){ //preenta icono de avanzar solo cuando llega hasta nivel de municipio
						cadena+= '	<div idref="'+item.cvegeo+'" class="generictheme-geo-seemore">';
						cadena+= '		<div class="generictheme-geoEdo-icon-seemore sprite-generictheme-forward"></div>';
						cadena+= '	</div>';
					}
					cadena+= '</div>';
			 }
			$('#generictheme_geo_content').html(cadena);
			
			$('.generictheme-geoEdo-icon').each(function(){
				$(this).click(function(e){
					var idref = $(this).attr('idref');
					obj.currentData.strats = 5; 
					obj.selGeoItem(idref);
					e.stopPropagation();
				})
				
			});
			
		}
	}
	obj.getGeoListItems(function(list){
		printList(list);
		obj.printGeoTypes(obj.currentData.geoSelected); 
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
	 
	 /*
	 if(gsel.indexOf(idgeo) >= 0){
		 if(idgeo != '00'){
			//cData.geoType = 'mun';
			if(gsel.length > 1){
				gsel.splice(gsel.indexOf(idgeo),1);
				obj.hasChanged = true;
				obj.currentData.geoSelected = gsel;
				
				obj.updateHeader();
				
				obj.prepareTheme();
			}
		 }
	 }else{
		 if(gsel[0] == '00' || idgeo == '00'){
			 gsel = [];
			// cData.geoType = 'edo';
	 	 }
		 if(idgeo != '00')
			// cData.geoType = 'mun';
		 
		 obj.gotoExtent(idgeo);
		 obj.currentData.geoSelected = gsel;
		 
		 gsel.push(idgeo);
		 obj.hasChanged = true;
		 obj.updateHeader();
		 
		 
		 obj.prepareTheme();
	 }
	 //adjust geoType
	 //obj.printGeoTypes();	 
	 */
	 obj.printGeoList('refresh');
	 
 },
 getGeoList:function(parent,func){
	  var obj = this;
	  var ds = obj.options.config.dataSources;
	  
	  var idGeo = parent;
	  /*var func = geo.callback;*/
	  
	  var obj = this;
	  var dataSource = $.extend(true,{},ds.geolist);
	  obj.getData(dataSource,{cvegeo:idGeo,zm:false},function(data){
		  if(data.response.success){
				if ($.isFunction(func)){
					func(data.data);
			    }
		  }
	  });
  },
spinner:function(option){
	var obj = this;
	if(option == 'show'){
		if(!$('#generictheme_spinner_panel').attr('id')){
			var w = obj.element.width();
			var h = obj.element.height();
			var cadena = '<div id="generictheme_spinner_panel" class="generictheme-spinner-panel" count="1" style="width:'+w+'px;height:'+h+'px">';
				cadena+= '	<div class="ui-widget-overlay generictheme-block-overlay"></div>';
				cadena+= '	<div class="generictheme-spinner-image-container"><span class="generictheme-spinner-image"></div>';
				cadena+= '<div>';

			obj.element.append(cadena);	
		}else{
			var count = parseInt($('#generictheme_spinner_panel').attr('count'),10);
			$('#generictheme_spinner_panel').attr('count',count+1);

		}
	}else{
		if($('#generictheme_spinner_panel').attr('id')){
			var count = parseInt($('#generictheme_spinner_panel').attr('count'),10);
			if(count > 1){
				$('#generictheme_spinner_panel').attr('count',count-1);	
			}else{
				$('#generictheme_spinner_panel').remove();
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
		cadena+='<div idstrat="'+strat.stratum+'" class="generictheme-strat-item-selector '+((x=='0')?'selected':'')+'" style="width:'+w+'%">';
		cadena+='	Estrato '+strat.stratum;
		cadena+='	<span class="generictheme-strat-selector-color" style="background-color:rgb('+strat.rgb.split(' ').join()+')">';
		cadena+='</div>';
	}
	
		cadena+= '<div class="generictheme-strat-table-container">';
		cadena+= '	<table id="generictheme_strat_table" class="generictheme-strat-table" strat="e1" width="100%">';
		cadena+= '	<tr class="generictheme-table-head"><td>'+geoType+'</td><td align="right">Valor</td></th></tr>';
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
	
	
	cadena+='<div class="generictheme-strat-table-export-container">';
	//Salto a grafica
	cadena+='<div id="generictheme_switchtograph_btn" class="generictheme-switchtograph-btn sprite-generictheme-graph-mini"></div>';
	
	
	var exportList = obj.settings.exportTypes;
	for(var x in exportList){
		cadena+='<div idref="'+exportList[x]+'" class="generictheme-export-type-item sprite-generictheme-doc-'+exportList[x]+'"></div>';
	}
	cadena+='</div>';
	
	
	$('#generictheme_info_content').html(cadena);
	
	
	//area de grafica
	cadena = '<div id="generictheme_info_graph_container" class="generictheme-info-graph-container">Cargando...</div>'
	cadena+=' <div id="generictheme_switchtodata_btn" class="generictheme-switchtograph-btn sprite-generictheme-grid"></div>';
	$('#generictheme_info_graph').html(cadena);
	
	
	
	
	$('#generictheme_switchtograph_btn').click(function(){
		$('#'+obj.id).attr('infopanel','graph');
	});
	$('#generictheme_switchtodata_btn').click(function(){
		$('#'+obj.id).attr('infopanel','data');
	});
	
	
	$('.generictheme-export-type-item').each(function(){
		$(this).click(function(){
			var idref = $(this).attr('idref');
			obj.exportStrats(idref);
		})
	})
	
	
	
	$('.generictheme-strat-item-selector').each(function(){
		$(this).click(function(){
			var idstrat = $(this).attr('idstrat');
			$('.generictheme-strat-item-selector.selected').removeClass('selected');
			$(this).addClass('selected');
			$('#generictheme_strat_table').attr('strat','e'+idstrat);
		});
	});
	
	
	
	//Print min max
	obj.updateRampStrat();
	$('#generictheme-year-min').html(obj.formatMoney(theme.min)); //using sugar format
	$('#generictheme-year-max').html(obj.formatMoney(theme.max));
	  //generictheme-year-max
	
	obj.createGraph();
	
	
},
//print panel config
  createRampColor:function(ramp){
	  	var obj = this;
		var colorRamps = obj.settings.colorRamps;
		var currentRamp = obj.currentData.colors;
		var colors = ramp.colors;
		var snum = obj.settings.numStrats;
		var cadena= '<div idref="'+ramp.id+'" class="generictheme-strats-colorRamp" '+((currentRamp.id == ramp.id)?'selected="selected"':'')+'>';
			for(var x in colors){
				var width = 100/snum;
				cadena+='<div class="generictheme-strats-ramp-color" style="background-color:'+colors[x]+';width:'+width+'%"></div>';	
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
			 cadena+= '<div id="'+obj.id+'_graph_strat'+'" class="generictheme-graph-strat"></div>'; 
			 cadena+= '<div class="generictheme-strats-transparency-container">';
			 cadena+= '		<div class="generictheme-strats-transparency-title">Transparencia</div>';
			 cadena+= '		<div id="generictheme_strats_trasparencyControl" class="generictheme-strats-transparency-tool"></div>';
			 cadena+= '</div>';

			 cadena+= '<div class="generictheme-theme-normalInfo generictheme-theme-info">';
			 cadena+= '		<div><b>Total:</b><label>'+obj.formatMoney(cd.theme.indicator)+'<label></div>';
			 cadena+= '		<div><b>Elementos:</b><label>'+obj.formatMoney(cd.theme.n)+'</label></div>';
			 cadena+= '		<div><b>D.Estd:</b><label>'+obj.formatMoney(cd.theme.sd)+'<label></div>';
			 cadena+= '</div>';

			 cadena+= '<div class="generictheme-theme-mainInfo generictheme-theme-info">';
			 cadena+= '		<div><b>Media:</b><label>'+obj.formatMoney(cd.theme.mean)+'<label></div>';
			 cadena+= '		<div><b>Mediana:</b><label>'+obj.formatMoney(cd.theme.median)+'<label></div>';
			 cadena+= '		<div><b>Moda:</b><label>'+obj.formatMoney(cd.theme.mode)+'<label></div>';
			 cadena+= '</div>';


			 cadena+=	'<div class="generictheme-strat-method generictheme-animated">';

			var methods = obj.settings.methods; //print methods
		 	 var widthMethod = Math.floor(100/(methods.length))-2;
			 for(var x in methods){
				var method = methods[x];
				cadena+=	'<div style="width:'+widthMethod+'%" val="'+method.name+'" class="generictheme-strat-item '+((method.name == cd.method)?'selected':'')+'">'+method.title+'</div>';
			 }
			 cadena+=	'</div>';

		   var minStrats = obj.settings.minStrats;
		   var maxStrats = obj.settings.maxStrats;

			cadena+= '	<div class="generictheme-strats-strat-title">No.Estratos</div>';
			cadena+= '	<div id="generictheme_strats_data_config_strats" class="generictheme-strats-data-config-strats">';
					   for(var x = minStrats; x <= maxStrats;x++){
							cadena+= '<div idref="'+x+'" class="generictheme-strats-strat-item '+((x == cd.strats)?'selected':'')+'">'+x+'</div>';
					   }
			cadena+= '	</div>';


		/*	 cadena+=	'<div class="generictheme-years generictheme-animated">';
			 cadena+=	'	<div class="generictheme-years-title">Año</div>';

			 var years = obj.settings.years;  
			 for(var x in years){
				var year = years[x];
				cadena+=	'<div val="'+year+'" class="generictheme-years-item '+((year == cd.year)?'selected':'')+'">'+year+'</div>';
			 } 
			 cadena+=	'</div>';*/


			//cadena+=	'<div class="generictheme-config-titles"><label></label></div>'; 
			cadena+=	'<div class="generictheme-strats-currentRamps-container">';

			 for(var x in colorRamps){
					var ramp = colorRamps[x];   
					cadena+= obj.createRampColor(ramp); 
			 }
			 cadena+=	'</div>';
	 }
	 
	 $('#generictheme_conf_container').html(cadena);
	 
	$('.generictheme-strats-strat-item ').each(function(index, element) {
		$(this).click(function(e){
			$('.generictheme-strats-strat-item.selected').removeClass('selected');
			$(this).addClass('selected');
			obj.showStratsMessage = true;
			obj.currentData.strats = parseInt($(this).attr('idref'),10);
			obj.hasChanged = true;
			obj.updateHeader();
			
			obj.prepareTheme();
		});
	});
	 
	 
	 $( "#generictheme_strats_trasparencyControl" ).slider({
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
	 
	 $('.generictheme-strats-colorRamp').each(function(){
			$(this).click(function(){
				
				$('.generictheme-strats-colorRamp[selected=selected]').each(function(index, element) {
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
	 
	 
	 
	 $('.generictheme-strat-item').each(function(index, element) {
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.generictheme-strat-item.selected').removeClass('selected');
				$(this).addClass('selected');
				
				obj.currentData.method = val;
				obj.hasChanged = true;
				obj.updateHeader();
				
				obj.prepareTheme();
				
		    });  
      });
	 /* $('.generictheme-years-item').each(function(index, element) { 
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.generictheme-years-item.selected').removeClass('selected');
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
		if(cData.geoSelected != '00'){  //se tematiza solo cuando no es 00
			
			if(!cData.geoSelected)cData.geoSelected = ['00'];

			var dataSource = $.extend(true,{},obj.options.config.dataSources.theme);
			var params = {
					ent:'23',//cData.geoSelected.join(),
					variable:cvar,
					estratos:cData.strats,
				  //year:cData.year,
					level:cData.geoType,
					tipoConsulta:cData.method,

				}	
			var themeParams = null;

			obj.getData(dataSource,params,function(data){
					obj.hasChanged = false;
					obj.updateHeader();

					if(data.response.success){
						var vals = data.data;
						var idTheme = data.data.id;

						var themeParams = {'LAYERS':'d501,d502'}

						themeParams['MAPAESTATAL'] = 0;//(cData.geoType == 'edo')?idTheme:0;
						themeParams['MAPAMUNICIPAL'] = 0;//(cData.geoType == 'mun')?idTheme:0;;
						themeParams['MAPALOCALIDAD'] = 0;
						themeParams['MAPAAGEB'] = 0;


						themeParams['MAPADISTRITO'] = idTheme; //temp
						themeParams['MAPASECCION'] = 0;

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
		  values.push(['Fuente:'+obj.settings.source[typeVar]]);
		  
		  var ds = $.extend({},true,obj.options.config.dataSources.exportData);
		  var params = {title:'Detalle de Estratos',columns:columns,values:values};
		  obj.getData(ds,params,function(data){
			  if(data && data.response.success){
			  	var url = ds.urlGet+'/'+type+'/'+data.data.id;
			  	window.location.assign(url);
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
		var file='<iframe src="'+doc+'" width="100%" height="370px" class="generictheme-metadata-frame"></iframe>';
		 
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
			  cadena+= '<div class="generictheme-stratum-detail-item">';
			  cadena+= '	  	<div class="generictheme-stratum-detail-item-color" style="background-color:'+color+';"></div>';
			  cadena+= '	  	<div class="generictheme-stratum-detail-item-info">';
			  cadena+= '			<div class="generictheme-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="generictheme-stratum-detail-item-info-label">Estrato</label>';
			  cadena+= '				<label class="generictheme-stratum-detail-item-info-value">'+item.stratum+'</label>';
			  cadena+= '			</div>';
			  cadena+= '			<div class="generictheme-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="generictheme-stratum-detail-item-info-label">Frecuencia</label>';
			  cadena+= '				<label class="generictheme-stratum-detail-item-info-value">'+(item.cvegeo.length)+'</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
		  }
		  //elementos extra
		      cadena+= '<div class="generictheme-stratum-detail-item">';
			  cadena+= '	  	<div class="generictheme-stratum-detail-item-color generictheme-confiden-bg"></div>';
			  cadena+= '	  	<div class="generictheme-stratum-detail-item-info">';
			  cadena+= '			<div class="generictheme-stratum-detail-item-info-vals special">';
			  cadena+= '				<label class="generictheme-stratum-detail-item-info-label">Confidencialidad</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
			  cadena+= '<div class="generictheme-stratum-detail-item">';
			  cadena+= '	  	<div class="generictheme-stratum-detail-item-color" style="background-color:#FFF;border:1px solid #adadad"></div>';
			  cadena+= '	  	<div class="generictheme-stratum-detail-item-info">';
			  cadena+= '			<div class="generictheme-stratum-detail-item-info-vals special">';
			  cadena+= '				<label class="generictheme-stratum-detail-item-info-label">Sin dato</label>';
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
	  
		  if(!$('#generictheme-Info-dialog').attr('id')){
			  var cadena = '<div title="'+title+'" id="generictheme-Info-dialog"><div id="generictheme-html-doc"></div></div>';
			  $('#panel-center').append(cadena);
			  $('#generictheme-Info-dialog').dialog({
				    dialogClass: 'generictheme-Info-dialogContainer',
					width:550,
					height:450,
				  	modal:true,
					close: function(event, ui)
					{
						$(this).dialog('destroy').remove();
					}
			  });
		  }else{
			  $('#generictheme-Info-dialog').html('<div id="generictheme-html-doc"></div>').dialog();
		  }
	  
	  	  $('#generictheme-html-doc').load(url);
   },
  	
//Control de ficha *********************************************************************************************************************
	onIdentify:function(identifyData){
		var obj = this;
		var pos = identifyData.pos;
		var cData = obj.currentData;
		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true,{},ds.infoPoint);
		
		var types = [null,'distrito','seccion'];
		var params = {
				cvegeo:cData.geoSelected[0],
				level:types[parseInt(cData.geoType)],
				point:'POINT('+pos.lon+' '+pos.lat+')'
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
	},
    getCardData:function(geo,func){
		var obj = this;
		var cData = obj.currentData;
		
		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true,{},ds.getCardValues);

		var params = {
				id:geo.cvegeo,
				level:cData.geoType
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
		
		if($('#toolgenerictheme_info').attr('id'))
			$('#toolgenerictheme_info').remove();
		
		
		var cadena = '<div id="toolgenerictheme_info" title="'+dialogTitle+'"><div id="toolgenerictheme_info_content"></div></div>';
			$("#panel-center").append(cadena);
		
		$('#toolgenerictheme_info').dialog({
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
		
		cadena = '<div id="generictheme_card_header"></div>';
		cadena+= '<div class="generictheme-card-title-indicators">Indicadores</div>';
		cadena+= '<div id="generictheme_card_indicators_container" class="generictheme-animated">';
		cadena+= '	<div id="generictheme_card_indicators_closebtn"><span class="sprite-generictheme-sleft"></span><span class="sprite-generictheme-sright"></span></div>';
		cadena+= '	<div id="generictheme_card_indicators"></div>';
		cadena+= '</div>';
		cadena+= '<div id="generictheme_card_values" class="generictheme-animated"></div>';
		cadena+= '<div id="generictheme_card_toolbar"></div>';
		
		$('#toolgenerictheme_info_content').html(cadena);
		
		
		//encabezado
		cadena = '<table class="generictheme-table-color">';
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
		$('#generictheme_card_header').html(cadena);
		
		//menú lateral
		var count = 1;
		cadena = '<ul>';
		for(var x in list){
			var item = list[x];
			cadena+= '<li class="'+((x==0)?'active ':'')+'geoelectoral-card-var generictheme-animated" pos="'+x+'"><div idref="'+item.field+'" class="ui-icon ui-icon-info generictheme-card-varid"></div><label>'+item.label+'</label></li>';
			count++;
		}
		cadena+= '</ul>'
		$('#generictheme_card_indicators').html(cadena);
		
		$('.generictheme-card-varid').each(function(){
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
			cadena+= '<div id="geoelectoral_ind_'+x+'" class="generictheme-card-block"><div pos="'+x+'" class="generictheme-card-title">'+count+'. '+item.label+'</div>';
			dataExport.push([item.label]);
			cadena+= 	'<div class="generictheme-card-blockcontent">';
			cadena+= 	'<table class="generictheme-table-color"><tbody>';
			
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
		$('#generictheme_card_values').html(cadena);
		
		
		//Exportación
		var exportList = obj.settings.exportTypes;
		cadena = '';
		for(var x in exportList){
			cadena+='<div idref="'+exportList[x]+'" class="generictheme-card-export-type-item sprite-generictheme-doc-'+exportList[x]+'"></div>';
		}
		//inclusión de descarga nacional
		var doc = obj.options.config.settings.cardIndicatorNalDownload;
		var docPDF = obj.options.config.settings.cardIndicatorNalDownloadPDF;
		cadena+='<div class="generictheme-card-export-link-item"><label>Nacional </label><a href="'+doc+'" target="_blank"><span class="sprite-generictheme-doc-xls"></span></a><a href="'+docPDF+'" target="_blank"><span class="sprite-generictheme-doc-pdf-mini"></span></a></div>';
		
		cadena+='</div>';
		
		$('#generictheme_card_toolbar').html(cadena);
		
		$('.generictheme-card-export-type-item').each(function(){
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
	  var params = {title:'Información Geoelectoral',columns:['',''],values:edata};
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
			$('#generictheme_card_values').animate({scrollTop:top}, 500, 'swing', function() { 
			});
		}
		$('.generictheme-card-title').each(function(){
			var top = $(this).position().top;
			if(count == 0)
				fix = top;
			scrollpos.push(top-fix);
			count++;
		});
		
		$('#generictheme_card_values').scroll(function(){
			var scroll = $('#generictheme_card_values').scrollTop();
			for(var x in scrollpos){
				if((scrollpos[x]-scroll)>=0){
					if((scrollpos[x]-scroll) > $('#generictheme_card_values').height())
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
				$('#generictheme_card_values').animate({scrollTop:top}, 500, 'swing', function() { 
				});
				*/
				
				//$('#generictheme_card_values').scrollTop(top);
			})
		});
		
		$('#generictheme_card_indicators_closebtn').unbind('click');
		$('#generictheme_card_indicators_closebtn').click(function(){
			var status = $('#toolgenerictheme_info_content').attr('status');
			if(status != 'collapsed'){
				$('#toolgenerictheme_info_content').attr('status','collapsed');
			}else{
				$('#toolgenerictheme_info_content').attr('status','expanded');
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
	
		
		Highcharts.chart('generictheme_info_graph_container', {
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
				data: dataSeries
			}]
		});
		
	},
	changeClassCSS:function(){
		var obj = this;
		var colors = obj.options.config.settings.uiColors;
		var path = obj.options.config.settings.path;
		var colorLight = colors[0];
		var colorMed = colors[1];
		
		//light colors
		var lcolors = [
				{sel:'.generictheme-tab:hover',style:'background-color:'}, //boton principal izq sobre
				{sel:'.generictheme-strat-item-selector:hover',style:'background-color:'}, //tabulado de estratos sobre
				{sel:'.generictheme-strat-item-selector.selected',style:'background-color:'}, //tabulado de estratos, seleccionado
				{sel:'.generictheme-strat-item.selected',style:'background-color:'}, //metodo de estratificacion seleccionado
				{sel:'.generictheme-strats-strat-item.selected',style:'background-color:'}, //numero de estratos seleccionado
				{sel:'.generictheme-geotype-item.selected',style:'background-color:'} //tipo de seleccion geografica activada
		];
		var mcolors = [
				{sel:'.generictheme-table-head',style:'background-color:'}, //encabezado de tabla
				{sel:'.custom-generictheme[seltab=geo] #generictheme_tab_geo',style:'background-color:'}, //seleccionado opcion geo
				{sel:'.custom-generictheme[seltab=var] #generictheme_tab_vars',style:'background-color:'},//seleccionado opcion vars
				{sel:'.custom-generictheme[seltab=graph] #generictheme_tab_graph',style:'background-color:'}, //seleccionado opcion estratos
				{sel:'.custom-generictheme[seltab=info] #generictheme_tab_info',style:'background-color:'}, //seleccionado opcion detalle
				{sel:'.generictheme-strats-currentRamps-container .generictheme-strats-colorRamp[selected=selected]',style:'border: 3px solid '} //rampa de color seleccionada
				]
		for(var x in lcolors){
			var style = lcolors[x].style+colorLight+' !important;';
			obj.createCSSSelector(lcolors[x].sel, style);	
			//obj.createCSSSelector(lcolors[x], 'background-color:'+colorLight);	
		}
		for(var x in mcolors){
			var style = mcolors[x].style+colorMed+' !important;';
			obj.createCSSSelector(mcolors[x].sel,style);	
			//obj.createCSSSelector(lcolors[x], 'background-color:'+colorLight);	
		}
	},
	createCSSSelector:function(selector, style) {
	  if (!document.styleSheets) return;
	  if (document.getElementsByTagName('head').length == 0) return;

	  var styleSheet,mediaType;

	  if (document.styleSheets.length > 0) {
		for (var i = 0, l = document.styleSheets.length; i < l; i++) {
		  if (document.styleSheets[i].disabled) 
			continue;
		  var media = document.styleSheets[i].media;
		  mediaType = typeof media;

		  if (mediaType === 'string') {
			if (media === '' || (media.indexOf('screen') !== -1)) {
			  styleSheet = document.styleSheets[i];
			}
		  }
		  else if (mediaType=='object') {
			if (media.mediaText === '' || (media.mediaText.indexOf('screen') !== -1)) {
			  styleSheet = document.styleSheets[i];
			}
		  }

		  if (typeof styleSheet !== 'undefined') 
			break;
		}
	  }

	  if (typeof styleSheet === 'undefined') {
		var styleSheetElement = document.createElement('style');
		styleSheetElement.type = 'text/css';
		document.getElementsByTagName('head')[0].appendChild(styleSheetElement);

		for (i = 0; i < document.styleSheets.length; i++) {
		  if (document.styleSheets[i].disabled) {
			continue;
		  }
		  styleSheet = document.styleSheets[i];
		}

		mediaType = typeof styleSheet.media;
	  }

	  if (mediaType === 'string') {
		for (var i = 0, l = styleSheet.rules.length; i < l; i++) {
		  if(styleSheet.rules[i].selectorText && styleSheet.rules[i].selectorText.toLowerCase()==selector.toLowerCase()) {
			styleSheet.rules[i].style.cssText = style;
			return;
		  }
		}
		styleSheet.addRule(selector,style);
	  }
	  else if (mediaType === 'object') {
		var styleSheetLength = (styleSheet.cssRules) ? styleSheet.cssRules.length : 0;
		for (var i = 0; i < styleSheetLength; i++) {
		  if (styleSheet.cssRules[i].selectorText && styleSheet.cssRules[i].selectorText.toLowerCase() == selector.toLowerCase()) {
			styleSheet.cssRules[i].style.cssText = style;
			return;
		  }
		}
		styleSheet.insertRule(selector + '{' + style + '}', styleSheetLength);
	  }
	}
  
});


























