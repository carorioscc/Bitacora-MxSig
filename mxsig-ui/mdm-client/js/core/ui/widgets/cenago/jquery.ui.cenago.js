$.widget( "custom.cenago", {
  // default options
  options: {
	  config:null,
  },
  currentData:null,
  hasChanged:false,
  // the constructor
  _create: function() {
	var obj = this;
	obj.id = obj.element.attr('id');
	  
	obj.currentData = obj.options.config.config.startingData;
	obj.settings = obj.options.config.config.settings;
	  
	this.element
	  // add a class for theming
	  .addClass( "custom-cenago" ).attr('collapsed','true').attr('seltab','var').attr('changed','false').attr('geotype',obj.currentData.geoType)
	  // prevent double click to select text
	  .disableSelection();
	 this.id = this.element.attr('id');
	 
	obj.element.addClass('no-print');
	  
	obj.options.onStart(); 
	obj.createUI();
	
	//inicia tema al iniciar
	obj.prepareTheme();
	  
	obj.settings.docPath+'/'+obj.settings.mainDoc  
	  
	var url = obj.settings.docPath+'/'+obj.settings.bootDialog;
	obj.openDialog(url, 'Censos Nacionales de gobierno');
	this._refresh();
  },

  // called when created, and later when changing option
  _refresh: function() {
	  // trigger a callback/event
	  
  },

  // events bound via _on are removed automatically
  // revert other modifications here
  _destroy: function() {
	this.onClose();
	this.element
	  .removeClass( "custom-cenago" )
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
		cadena += '<div class="cenago-header-colors-years-label"><div id="cenago-year-min" class="cenago-header-year-label-left"></div><div id="cenago-year-max" class="cenago-header-year-label-right"></div></div>';

		var colors = obj.currentData.colors.colors;
		for (var x in colors) {
			if(parseInt(x,10) < strats){
				var color = colors[x];
				cadena += '<div class="cenago-header-colors-item" style="background-color:' + color + ';width:' + Math.floor(100 / strats) + '%"></div>';
			}
		}
		cadena += '</div>';
		
		$('#cenago_color_container').html(cadena);

	},
   createUI:function(){
	  var obj = this;
	  var cd = obj.currentData;  
	   
	  obj.printGeoList();
	  var 	cadena = '<div id="cenago_header" class="cenago-header cenago-resize cenago-transition">';
			cadena+=	'<div id="cenago_header_logoLeft" idref="" class="cenago-logo-left"></div>';
	   		cadena+=	'<div id="cenago_header_title" class="cenago-truncate-text"><div id="cenago_var_title">Seleccione una variable</div><div id="cenago_subvar_title"></div></div>';
	   		cadena+=	'<div class="cenago-header-btns">';
	   		cadena+=	'	<div id="cenago_header_btnRight" idref="" class="cenago-header-btn">';
			cadena+=	'		<div id="cenago_header_btnRight_modify" idref="" class="cenago-header-btn-inner sprite-cenago sprite-cenago-modify"></div>';
	   		cadena+=	'		<div id="cenago_header_btnRight_ok_nm" idref="" class="cenago-header-btn-inner sprite-cenago sprite-cenago-ok-nm"></div>';
			cadena+=	'		<div id="cenago_header_btnRight_ok_n" idref="" class="cenago-header-btn-inner sprite-cenago sprite-cenago-ok-m"></div>';
	   		cadena+=	'	</div>';
	   		cadena+=	'	<div id="'+obj.id+'_header_btnRight_close" idref="" class="cenago-header-btnRight-close cenago-header-btn-inner sprite-cenago sprite-cenago-down"></div>';
	   		cadena+=	'</div>';
			cadena+=	'<div class="cenago-header-colors" id="cenago_color_container">';
	   		/*cadena+=	'<div class="cenago-header-colors-years-label"><div id="cenago-year-min" class="cenago-header-year-label-left"></div><div id="cenago-year-max" class="cenago-header-year-label-right"></div></div>';

			var colors = obj.currentData.colors.colors;
	   		for(var x in colors){
				var color = colors[x];
				cadena+=	'<div class="cenago-header-colors-item" style="background-color:'+color+';width:'+Math.floor(100/colors.length)+'%"></div>';
			}
	   
			cadena+=	'</div>';*/
	   
	  		cadena+= '</div>';
			cadena+= '<div id="cenago_panels_container" class="cenago-panels-container cenago-animated" panel="vars">';
	   		cadena+= '	<div id="cenago_container" class="cenago-container cenago-tab-container" parents="false" typeselection="'+obj.currentData.typeVarSelection+'">';

	   		cadena+= '		<div id="cenago_geo_content_options" class="cenago-geo-content-options cenago-resize cenago-animated">';
			cadena+= '			<div idref="edomun" id="cenago_tool_total" class="cenago-tool-option cenago-tool-total">TOTAL</div>';
	   		cadena+= '			<div idref="edo" id="cenago_tool_edo" class="cenago-tool-option cenago-tool-total">EDO</div>';
	   		cadena+= '			<div idref="mun" id="cenago_tool_mun" class="cenago-tool-option cenago-tool-total">MUN</div>';
			cadena+= '		</div>';
			
	   		cadena+= '		<div id="cenago_content" class="cenago-resize cenago-animated cenago-content"></div>';
	   		cadena+= '		<div id="cenago_bk_btn" class="cenago-resize cenago-animated cenago-bk-btn"><div class="cenago-bk-btn-icon sprite-cenago sprite-cenago-bback"></div></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="cenago_geo_container" class="cenago-geo-container cenago-tab-container">';
			cadena+= '		<div id="cenago_geo_bk_btn" class="cenago-resize cenago-animated cenago-geo-bk-btn"><div class="cenago-geo-bk-btn-icon sprite-cenago sprite-cenago-bback"></div></div>';
	   		cadena+= '		<div id="cenago_geo_content" class="cenago-resize cenago-animated cenago-geo-content"></div>';
	   		cadena+= '		<div id="cenago_geo_type" class="cenago-resize cenago-animated cenago-geo-type">';
			cadena+= '		</div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="cenago_conf_container" class="cenago-conf-container cenago-tab-container">';
			cadena+= '		<div id="cenago_conf_content" class="cenago-resize cenago-animated cenago-conf-content"></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="cenago_info_container" class="cenago-info-container cenago-tab-container">';
			cadena+= '		<div id="cenago_info_content" class="cenago-resize cenago-animated cenago-info-content"></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="cenago_docs_container" class="cenago-docs-container cenago-tab-container">';
			cadena+= '	</div>';
	   		cadena+= '</div>';
	   
	   		cadena+= '<div id="cenago_toolbar_container" class="cenago-toolbar-container cenago-animated">';
	   		cadena+= '	<div id="cenago_tab_vars" idref="var" class="cenago-resize cenago-tab"><div class="sprite-cenago-vars"></div><div>Actividad</div></div>';
	   		cadena+= '	<div id="cenago_tab_geo" idref="geo" class="cenago-resize cenago-tab"><div class="sprite-cenago-geo"></div><div>Geográfico</div></div>';
	   		cadena+= '	<div id="cenago_tab_graph" idref="graph" class="cenago-resize cenago-tab"><div class="sprite-cenago-graph"></div><div>Estratos</div></div>';
	   		cadena+= '	<div id="cenago_tab_info" idref="info" class="cenago-resize cenago-tab"><div class="sprite-cenago-binfo"></div><div>Detalle</div></div>';
	   		cadena+= '	<div id="cenago_tab_docs" idref="docs" class="cenago-resize cenago-tab"><div class="sprite-cenago-big-info"></div><div>Más información</div></div>';
	   		/*cadena+= '	<div id="cenago_tab_bottom_container" idref="info" class="cenago-tab-bottom-container cenago-animated">';
			cadena+= '		<a href="'+obj.settings.docPath+'/'+obj.settings.mainDoc+'" target="_blank"><div id="cenago_tab_pdfdoc" idref="pdfdoc" class="cenago-resize sprite-cenago-doc-pdf"></div></a>';
			cadena+= '	</div>';*/
			cadena+= '</div>';

			
	  obj.element.html(cadena);
	   
	  //obj.updateRampStrat(); 
	  	
	   $('.cenago-tool-option').each(function(){
		  $(this).click(function(){
			 var idref = $(this).attr('idref');
			 
			 if(idref != obj.currentData.typeVarSelection){
				obj.hasChanged = true;	  
				$('#cenago_container').attr('typeselection',idref);
				 obj.currentData.typeVarSelection = idref;
				 obj.updateHeader();
				 
				 obj.prepareTheme();
			 }
		  });
	   });
	   
	   $('#'+obj.id+'_header_btnRight_close').click(function(){
		   obj.cancelModify();
	   });
	   
	   $('.cenago-tab').each(function(){
		   $(this).click(function(e){
			   var idref = $(this).attr('idref');
			   obj.element.attr('seltab',idref);
			   e.stopPropagation();
		   })
	   });
	   
	   
	  $('#cenago_header').click(function(e){
			  if($('#cenago_panels_container').height() == 0){
				obj.openConfig();
				e.stopPropagation();
			  }
   	 });

	  
	  $('#cenago_header_btnRight').click(function(){
		  if($('#cenago_panels_container').height() == 0){
			obj.openConfig();
		  }else{
			obj.closeConfig();
		  }
	   });
	   $('#cenago_bk_btn').click(function(){
		   if(obj.currentData.tree.length > 1){
			  obj.currentData.tree.pop();
			  if(obj.currentData.tree.length <= 1){
				  $('#cenago_container').attr('parent','false');
			  }
			  obj.loadTree(obj.currentData.tree[obj.currentData.tree.length-1].id,true);
		   }
	   });
	  
	  obj.element.fadeIn();
	  obj.printAbout();
	  
  },
//print geoTypes
 printAbout:function(){
		var obj = this;
		var cadena = '';
		cadena+= '<table class="cenago-info-table" width="100%" border="0">';
		cadena+= '	<tbody>';
		/*
		cadena+= '		<tr>';
		cadena+= '			<th colspan="3">Marco Censal cenago 2016</th>';
		cadena+= '		</tr>';
		*/
		cadena+= '		<tr>';
		cadena+= '  		<td width="45%">&nbsp;</td>';
		cadena+= '  		<td width="2%">&nbsp;</td>';
		cadena+= '  		<td width="53%">&nbsp;</td>';
		cadena+= '		</tr>';
		cadena+= '		<tr>';
		cadena+= '  		<td align="center" bgColor="#A9CDB9">Metodología</td>';
		cadena+= '  		<td>&nbsp;</td>';
		cadena+= '  		<td align="center" bgColor="#A9CDB9">Manual de usuario</td>';
		cadena+= '		</tr>';
		cadena+= '		<tr>';
		cadena+= ' 			<td align="center"><a href="'+obj.settings.docPath+'/'+obj.settings.mainDoc+'" target="_blank"><div class="cenago-resize sprite-cenago-doc-pdf"></div></a></td>';
		cadena+= '  		<td>&nbsp;</td>';
		cadena+= '  		<td align="center"><a href="'+obj.settings.docPath+'/'+obj.settings.manualDoc+'" target="_blank"><div class="cenago-resize sprite-cenago-doc-pdf"></div></a></td>';
		cadena+= '		</tr>';
		cadena+= '	</tbody>';
		cadena+= '</table>';
		$('#cenago_docs_container').html(cadena);
},
 openDialog: function (url, title) {
	    
		var obj = this;
		if (!$('#cenago-Info-dialog').attr('id')) {
			var cadena = '<div title="' + title + '" id="cenago-Info-dialog"><div id="cenago-html-doc"></div></div>';
			$('#panel-center').append(cadena);
			$('#cenago-Info-dialog').dialog({
				dialogClass: 'cenago-Info-dialogContainer',
				width: 620,
				height: 520,
				modal: true,
				close: function (event, ui) {
					$(this).dialog('destroy').remove();
				}
			});
		} else {
			$('#cenago-Info-dialog').html('<div id="cenago-html-doc"></div>').dialog();
		}
	    $('#cenago-html-doc').load(url);
 },
 printGeoTypes:function(geo){
	var obj = this;
	geo = geo[0]; 
	var cData = obj.currentData;
	var rules = cData.geoTypes;
	var sRule = null; //rule result
	
	for(var x in rules){ //se implementa un sistema de reglas bajo las cuales se define el tipo de dato seleccionado
		var rule = rules[x].rule;
		var rt = rule.substr(0,1);
		
		switch(rt){
			case 'e': //equal
				sRule = (geo == rule.substr(1,rule.length-1))?rules[x].list:null;
			break;
			case 'n': //number of chars
				sRule = (geo.length == parseInt(rule.substr(1,rule.length-1),10)?rules[x].list:null);
			break;
		}
		if(sRule)break;
	}
	 
	//Impresion de typos geograficos para la selección
	if(sRule){
		var ltypes = [];
		for(var x in sRule){
			var sItem = sRule[x];
			ltypes.push(sItem.id);
		}
		if(ltypes.indexOf(cData.geoType)< 0){
			cData.geoType =ltypes[0];
		}
		
		var cadena = '';
		for(var x in sRule){
			var sItem = sRule[x];
			var isSelected = (cData.geoType == sItem.id)?'selected':'';
			cadena+= '<div idref="'+sItem.id+'" class="cenago-geotype-item '+isSelected+'">'+sItem.val+'</div>';
		}
		
		$('#cenago_geo_type').html(cadena);
		$('.cenago-geotype-item').each(function(){
			$(this).click(function(){
				var idref = $(this).attr('idref');
				cData.geoType = idref;
				$('.cenago-geotype-item.selected').removeClass('selected');
				$(this).addClass('selected');
				obj.hasChanged = true;
				obj.updateHeader();
				
				obj.prepareTheme();
			});
		});
	}
 },
 //--------- Vars Display-----------------------------------------------------------------
  reloadTree:function(){
	  var obj = this;
	  var id = obj.currentData.tree[obj.currentData.tree.length-1].id;
	  obj.loadTree(id,true);
  },
  loadTree:function(id,reload){
	  var obj = this;
	  
	  var ds = obj.options.config.dataSources;
	  var dataSource = $.extend(true,{},ds.varlist);
	  
	  var params = {
		  		id:id
		  }

	  if(!reload || obj.currentData.tree.length == 0){
		  obj.getData(dataSource,params,function(data){
			   //si el elemento es una recarga no agrega el elemento al arbol de peticiones
			  if(data.data){
				   var list = data.data.registros || [];
				   
				   obj.currentData.tree.push({id:id,list:list});
				   //si hay mas de un nivel, coloca el boton de regreso
				   if(obj.currentData.tree.length > 1){
						$('#cenago_container').attr('parent','true');
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
	var postList = {
				'edomun':'estatales, municipales y demarcaciones territoriales ',
				'edo':'estatales',
				'mun':'municipales y demarcaciones territoriales ',
	}
	  
	obj.element.attr('geotype',obj.currentData.geoType);
	  
	//$('#cenago_var_title').html(_var.descripcion);
  
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
					if(svars.length == 1){
						svars[0] = svars[0]+' '+postList[typeVar];
					}
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
	  
	/*var label = (svars.length > 0)?
		svars.join('/')+' '+postList[typeVar] +' / '+_var.descripcion:
		_var.descripcion+' '+postList[typeVar];*/
	
	var label = (svars.length > 0)?
		svars.join('/')+' / '+_var.descripcion:
		_var.descripcion+' '+postList[typeVar];
	  
	  
	$('#cenago_subvar_title').html(label);//svars.join('/')+' '+postList[typeVar]);
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
			
			cadena+= '<div class="cenago-list-item" idref="'+item.id+'"><label>';
		   
		   if(item.metadata)
			cadena+= '	<span idref="'+item.variable+'" class="cenago-info-var-icon sprite-cenago-info"></span>';
			
		    cadena+= 	item.descripcion+'</label>';
			
			if(canSelect){
				if(!active){
					cadena+= '	<div idref="'+item.id+'" class="cenago-item-check sprite-cenago sprite-cenago-circle"></div>';   
				}else{
					cadena+= '	<div idref="'+item.id+'" class="cenago-item-check sprite-cenago sprite-cenago-ok"></div>';   
				}
			}
			
			if(item.subcat){
				cadena+= '<div idref="'+item.id+'" class="cenago-item-forward sprite-cenago sprite-cenago-forward"></div>';   
			}
			cadena+= '</div>';
	   }
	   $('#cenago_content').html(cadena);
	  
	   $('.cenago-info-var-icon').each(function(){
		  $(this).click(function(){
			  var idref = $(this).attr('idref');
			  obj.openMetadataVar(idref);
		  });
	   });
	  
	   $('.cenago-item-check').each(function(index, element) {
		   $(this).click(function(e){
				var idref = $(this).attr('idref');
				obj.selectVar(idref);
				e.stopPropagation();
		   })
		});
	   
	   $('.sprite-cenago-forward').each(function(index, element) {
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
		
		if(data.Estados){
			list.unshift({childs:false,cvegeo:"00",nombre:"Nacional"});
		}
		
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
					cadena+= '<div class="cenago-geoEdo-item" label="'+item.nombre.toLowerCase()+'" idparent="'+parent+'" idref="'+item.cvegeo+'" '+((isSelected)?'selected="selected"':'')+'>';
					cadena+= '	<div class="cenago-geoEdo-item-label">'+item.nombre+'</div>';
					cadena+= '	<div class="cenago-geoEdo-icon" idref="'+item.cvegeo+'">';
					cadena+= '		<div class="cenago-geoEdo-icon-sel sprite-cenago-circle"></div>';
					cadena+= '		<div class="cenago-geoEdo-icon-unsel sprite-cenago-ok"></div>';
					cadena+= '	</div>';
					
					//comentado VER MAS...
					if(item.childs && false){ //preenta icono de avanzar solo cuando llega hasta nivel de municipio
						cadena+= '	<div idref="'+item.cvegeo+'" class="cenago-geo-seemore">';
						cadena+= '		<div class="cenago-geoEdo-icon-seemore sprite-cenago-forward"></div>';
						cadena+= '	</div>';
					}
					cadena+= '</div>';
			 }
			$('#cenago_geo_content').html(cadena);
			
			$('.cenago-geoEdo-icon').each(function(){
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
	 
	 if(gsel.indexOf(idgeo) >= 0){
		 if(idgeo != '00'){
			cData.geoType = 'mun';
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
			 cData.geoType = 'edo';
	 	 }
		 if(idgeo != '00')
			 cData.geoType = 'mun';
		 
		 obj.gotoExtent(idgeo);
		 obj.currentData.geoSelected = gsel;
		 
		 gsel.push(idgeo);
		 obj.hasChanged = true;
		 obj.updateHeader();
		 
		 
		 obj.prepareTheme();
	 }
	 //adjust geoType
	 	 
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
		if(!$('#cenago_spinner_panel').attr('id')){
			var w = obj.element.width();
			var h = obj.element.height();
			var cadena = '<div id="cenago_spinner_panel" class="cenago-spinner-panel" count="1" style="width:'+w+'px;height:'+h+'px">';
				cadena+= '	<div class="ui-widget-overlay cenago-block-overlay"></div>';
				cadena+= '	<div class="cenago-spinner-image-container"><span class="cenago-spinner-image"></div>';
				cadena+= '<div>';

			obj.element.append(cadena);	
		}else{
			var count = parseInt($('#cenago_spinner_panel').attr('count'),10);
			$('#cenago_spinner_panel').attr('count',count+1);

		}
	}else{
		if($('#cenago_spinner_panel').attr('id')){
			var count = parseInt($('#cenago_spinner_panel').attr('count'),10);
			if(count > 1){
				$('#cenago_spinner_panel').attr('count',count-1);	
			}else{
				$('#cenago_spinner_panel').remove();
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
	
	for(var x in strats){
		var strat = strats[x];
		var w = (100/strats.length)-1;
		cadena+='<div idstrat="'+strat.stratum+'" class="cenago-strat-item-selector '+((x=='0')?'selected':'')+'" style="width:'+w+'%">';
		cadena+='	Estrato '+strat.stratum;
		cadena+='	<span class="cenago-strat-selector-color" style="background-color:rgb('+strat.rgb.split(' ').join()+')">';
		cadena+='</div>';
	}
	
		cadena+= '<div class="cenago-strat-table-container">';
		cadena+= '	<table id="cenago_strat_table" class="cenago-strat-table" strat="e1">';
		cadena+= '	<thead><th><td width="20px">ID</td><td width="200px">Nombre</td><td width="75px" align="right">Indicador</td></th></thead>';
	for(var x in detail){
		var itemx = detail[x];
		var strat = itemx.stratum;
		var stratDetail = itemx.cvegeo;
		for (var y in stratDetail){
			var itemy = stratDetail[y];
			
			cadena+= '<tr idstrat="e'+strat+'"><td width="47px">'+itemy.cvegeo+'</td><td width="163px">'+itemy.nombre+'</td><td width="75px" align="right">'+itemy.indicador+'</td></tr>';	
		}
	}
	cadena+='</table></div>';
	
	
	cadena+='<div class="cenago-strat-table-export-container">';
	var exportList = obj.settings.exportTypes;
	for(var x in exportList){
		cadena+='<div idref="'+exportList[x]+'" class="cenago-export-type-item sprite-cenago-doc-'+exportList[x]+'"></div>';
	}
	cadena+='</div>';
	
	
	$('#cenago_info_container').html(cadena);
	
	
	$('.cenago-export-type-item').each(function(){
		$(this).click(function(){
			var idref = $(this).attr('idref');
			obj.exportStrats(idref);
		})
	})
	
	
	
	$('.cenago-strat-item-selector').each(function(){
		$(this).click(function(){
			var idstrat = $(this).attr('idstrat');
			$('.cenago-strat-item-selector.selected').removeClass('selected');
			$(this).addClass('selected');
			$('#cenago_strat_table').attr('strat','e'+idstrat);
		});
	});
	
	
	
	//Print min max
	obj.updateRampStrat();
	$('#cenago-year-min').html(obj.formatMoney(theme.min)); //using sugar format
	$('#cenago-year-max').html(obj.formatMoney(theme.max));
	  //cenago-year-max
	
	
	
},
//print panel config
  createRampColor:function(ramp){
	  	var obj = this;
		var colorRamps = obj.settings.colorRamps;
		var currentRamp = obj.currentData.colors;
		var colors = ramp.colors;
		var snum = obj.settings.numStrats;
		var cadena= '<div idref="'+ramp.id+'" class="cenago-strats-colorRamp" '+((currentRamp.id == ramp.id)?'selected="selected"':'')+'>';
			for(var x in colors){
				var width = 100/snum;
				cadena+='<div class="cenago-strats-ramp-color" style="background-color:'+colors[x]+';width:'+width+'%"></div>';	
			}
			cadena+='</div>';
		return cadena; 
 },
 printConfig:function(){
	 var obj = this;
	 var cd = obj.currentData; 
	 var colorRamps = obj.settings.colorRamps;
	 
	 var cadena = '';
	 	
	 
	 
	 	 cadena+= '<div id="'+obj.id+'_graph_strat'+'" class="cenago-graph-strat"></div>'; 
	 	 cadena+= '<div class="cenago-strats-transparency-container">';
		 cadena+= '		<div class="cenago-strats-transparency-title">Transparencia</div>';
		 cadena+= '		<div id="cenago_strats_trasparencyControl" class="cenago-strats-transparency-tool"></div>';
		 cadena+= '</div>';
	 
	 	 cadena+= '<div class="cenago-theme-normalInfo cenago-theme-info">';
		 cadena+= '		<div><b>Total:</b><label>'+obj.formatMoney(cd.theme.indicator)+'<label></div>';
		 cadena+= '		<div><b>Elementos:</b><label>'+obj.formatMoney(cd.theme.n)+'</label></div>';
		 cadena+= '		<div><b>D.Estd:</b><label>'+obj.formatMoney(cd.theme.sd)+'<label></div>';
	 	 cadena+= '</div>';
		 
		 cadena+= '<div class="cenago-theme-mainInfo cenago-theme-info">';
	 	 cadena+= '		<div><b>Media:</b><label>'+obj.formatMoney(cd.theme.mean)+'<label></div>';
		 cadena+= '		<div><b>Mediana:</b><label>'+obj.formatMoney(cd.theme.median)+'<label></div>';
		 cadena+= '		<div><b>Moda:</b><label>'+obj.formatMoney(cd.theme.mode)+'<label></div>';
	 	 cadena+= '</div>';
	 
	 
	 	 cadena+=	'<div class="cenago-strat-method cenago-animated">';
	   
		 var methods = obj.settings.methods; //print methods
		 for(var x in methods){
			var method = methods[x];
			cadena+=	'<div val="'+method.name+'" class="cenago-strat-item '+((method.name == cd.method)?'selected':'')+'">'+method.title+'</div>';
		 }
		 cadena+=	'</div>';
	 
	   var minStrats = obj.settings.minStrats;
	   var maxStrats = obj.settings.maxStrats;
	
		cadena+= '	<div class="cenago-strats-strat-title">No.Estratos</div>';
		cadena+= '	<div id="cenago_strats_data_config_strats" class="cenago-strats-data-config-strats">';
				   for(var x = minStrats; x <= maxStrats;x++){
						cadena+= '<div idref="'+x+'" class="cenago-strats-strat-item '+((x == cd.strats)?'selected':'')+'">'+x+'</div>';
				   }
		cadena+= '	</div>';
	 	

		 cadena+=	'<div class="cenago-years cenago-animated">';
		 cadena+=	'	<div class="cenago-years-title">Año</div>';

		 var years = obj.settings.years;  //print years
		 for(var x in years){
			var year = years[x];
			cadena+=	'<div val="'+year+'" class="cenago-years-item '+((year == cd.year)?'selected':'')+'">'+year+'</div>';
		 } 
		 cadena+=	'</div>';
	 	 
	 
	 	//cadena+=	'<div class="cenago-config-titles"><label></label></div>'; 
	 	cadena+=	'<div class="cenago-strats-currentRamps-container">';
	 
		 for(var x in colorRamps){
				var ramp = colorRamps[x];   
				cadena+= obj.createRampColor(ramp); 
		 }
	 	 cadena+=	'</div>';
	 
	 
	 $('#cenago_conf_container').html(cadena);
	 
	$('.cenago-strats-strat-item ').each(function(index, element) {
		$(this).click(function(e){
			$('.cenago-strats-strat-item.selected').removeClass('selected');
			$(this).addClass('selected');
			obj.currentData.strats = parseInt($(this).attr('idref'),10);
			obj.hasChanged = true;
			obj.updateHeader();
			
			obj.prepareTheme();
		});
	});
	 
	 
	 $( "#cenago_strats_trasparencyControl" ).slider({
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
	 
	 $('.cenago-strats-colorRamp').each(function(){
			$(this).click(function(){
				
				$('.cenago-strats-colorRamp[selected=selected]').each(function(index, element) {
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
	 
	 
	 
	 $('.cenago-strat-item').each(function(index, element) {
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.cenago-strat-item.selected').removeClass('selected');
				$(this).addClass('selected');
				
				obj.currentData.method = val;
				obj.hasChanged = true;
				obj.updateHeader();
				
				obj.prepareTheme();
				
		    });  
      });
	  $('.cenago-years-item').each(function(index, element) {
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.cenago-years-item.selected').removeClass('selected');
				$(this).addClass('selected');
				
				cd.year = parseInt(val,10);
				obj.hasChanged = true;
				obj.updateHeader();
				
				obj.prepareTheme();
				
		    });  
      });
	 
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
		
		if(!cData.geoSelected)cData.geoSelected = ['00'];
		
		var dataSource = $.extend(true,{},obj.options.config.dataSources.theme);
		var params = {
				ent:cData.geoSelected.join(),//vals.geo.id,
				sector:0,
				variable:cvar,
				estratos:cData.strats,
				year:cData.year,
				level:cData.geoType,
				tipoConsulta:cData.method,
				total:cData.showTotal
		 	}
		var themeParams = null;
		
		obj.getData(dataSource,params,function(data){
				obj.hasChanged = false;
				obj.updateHeader();
			
				if(data.response.success){
					var vals = data.data;
					var idTheme = data.data.id;

					var themeParams = {'LAYERS':'d100,d101,d102,d109'}

					themeParams['MAPAESTATAL'] = (cData.geoType == 'edo')?idTheme:0;
					themeParams['MAPAMUNICIPAL'] = (cData.geoType == 'mun')?idTheme:0;;
					themeParams['MAPALOCALIDAD'] = 0;
					themeParams['MAPAAGEB'] = 0;
					
					obj.options.refreshMap(themeParams);
					obj.currentData.theme = vals;
					obj.printThemeDetail();
					obj.backupData =  obj.currentData;
					obj.checkThemeColor();
					if(obj.stratChanged)
						obj.options.systemMessage('Se modifico la configuración a '+obj.currentData.strats+' estratos',{width:240,height:120,title:'Información'});
					
					obj.stratChanged = false;
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
					'edomun':'estatales, municipales y demarcaciones territoriales',
					'edo':'estatales',
					'mun':'municipales y demarcaciones territoriales',
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
		var doc = path+'/'+geoType+'_'+_var+'.html';
		
		var file='<iframe src="'+doc+'" width="100%" height="255px" class="cenago-metadata-frame"></iframe>';
		 
		obj.options.systemMessage(file,{width:500,height:350,title:'Información'});
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
		var theme = data.theme;
		if(theme){
		  var detail = theme.detail;
		  var title = data.varActive.descripcion;  
		  var cadena = '';	
		 //var graphObj = obj.crateGraphData(detail,title);	 //grafica anterior, bloquedas por el momento Marzo 2017
		  for (var x in detail){
			  var item = detail[x];
			  cadena+= '<div class="cenago-stratum-detail-item">';
			  cadena+= '	  	<div class="cenago-stratum-detail-item-color" style="background-color:rgb('+(item.rgb.replace(/ /g,','))+');"></div>';
			  cadena+= '	  	<div class="cenago-stratum-detail-item-info">';
			  cadena+= '			<div class="cenago-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="cenago-stratum-detail-item-info-label">Estrato</label>';
			  cadena+= '				<label class="cenago-stratum-detail-item-info-value">'+item.stratum+'</label>';
			  cadena+= '			</div>';
			  cadena+= '			<div class="cenago-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="cenago-stratum-detail-item-info-label">Frecuencia</label>';
			  cadena+= '				<label class="cenago-stratum-detail-item-info-value">'+(item.cvegeo.length)+'</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
		  }
		  //elementos extra
		      cadena+= '<div class="cenago-stratum-detail-item">';
			  cadena+= '	  	<div class="cenago-stratum-detail-item-color cenago-confiden-bg"></div>';
			  cadena+= '	  	<div class="cenago-stratum-detail-item-info">';
			  cadena+= '			<div class="cenago-stratum-detail-item-info-vals special">';
			  cadena+= '				<label class="cenago-stratum-detail-item-info-label">Confidencialidad</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
			  cadena+= '<div class="cenago-stratum-detail-item">';
			  cadena+= '	  	<div class="cenago-stratum-detail-item-color" style="background-color:#FFF;border:1px solid #adadad"></div>';
			  cadena+= '	  	<div class="cenago-stratum-detail-item-info">';
			  cadena+= '			<div class="cenago-stratum-detail-item-info-vals special">';
			  cadena+= '				<label class="cenago-stratum-detail-item-info-label">Sin dato</label>';
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
  }
  
});