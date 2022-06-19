$.widget( "custom.ecoTool", {
  // default options
  options: {
	  edos:[],
	  config:null,
	  var_vals:null,
	  var_gs:[],
	  dataDource:null,
	  stratCount:5,
	  maxGeo:10,
	  noDataStrat:null,
	  colorRamps:[],
	  getResolution:function(){},
	  inGeoArea:function(){},
	  notInGeoArea:function(){},
	  onStart:function(){},
	  onClose:function(){},
	  refreshLayer:function(){},
	  systemMessage:function(msg){},
	  detectCollision:function(){}
  },
  configData:{
	maxHeight:550,
	minHeight:200,
	transparency:100,
	title:'Censos Económicos ',
	colorRamps:[
	]
  },
  data:{geolist:[]},
  collapsed:true,
  canResize:true,
  currentData:{},
  selectedData:{},
  infoData:null,
  serviceData:{
		sector:[],
		ssector:[]  
  },
  hasChanged:true,
  applyChangesTimer:null,
  getGeoInfo:function(geo){
	  var idGeo = geo.cvegeo;
	  var func = geo.callback;
	  var obj = this;
	  var dataSource = $.extend(true,{},obj.options.dataSource.getGeo);
	  	  dataSource.url+= '/'+idGeo;
	  obj.getData(dataSource,{},function(data){
		  if(data.response.success){
				if ($.isFunction(func)){
					func(data);
			    }
		  }
	  });
  },
  extentToCvegeo:function(id){
	 var obj = this;
	 obj.getGeoInfo({cvegeo:id,callback:function(data){
		 obj.options.extent(data.data.extent);
	  }});
  },
  getDataInfoVars:function(){
	  var obj = this;
	  var vals = obj.currentData;
	  var dataSource = $.extend(true,{},obj.options.dataSource.infoVars);
	  obj.getData(dataSource,{},function(data){
		  obj.infoData = data;
	  });
  },
  getVarsData:function(){
	  return this.infoData; 
  },
  openDialogVar:function(_var){
	  var obj = this; 
	  var data = obj.infoData.vars_vals.var_eco.list.concat(obj.infoData.vars_vals.var_relan.list);
	  var varInfo = null;
	  for(var x in data){
		 var item = data[x];  
		 if(item.id == _var)
		 	varInfo = item;
	  }
	
	  if(varInfo){
		  if(!$('#ecoTool-varInfo-dialog').attr('id')){
			  var cadena = '<div title="'+varInfo.nombre+'" id="ecoTool-varInfo-dialog">'+varInfo.html+'</div>';
			  $('#panel-center').append(cadena);
			  $('#ecoTool-varInfo-dialog').dialog({
				    dialogClass: 'ecoTool-varInfo-dialogContainer',
					width:450,
					height:400,
					close: function(event, ui)
					{
						$(this).dialog('destroy').remove();
					}
			  });
			  $('.ecoTool-varInfo-dialogContainer .ui-dialog-title').attr('title',varInfo.nombre);
		  }else{
			  $('#ecoTool-varInfo-dialog').html(varInfo.html).dialog({ title: varInfo.nombre});
			  $('.ecoTool-varInfo-dialogContainer .ui-dialog-title').attr('title',varInfo.nombre);
		  }
	  }
  },
  getGeoTypeCurrentSelection:function(){
	var obj = this;
	var list = obj.selectedData.selection.list;
	
	var geoTypes = {
			t2:'EDO',
		 	t5:'MUN',
		 	t9:'LOC',
		    t13:'AGEB'//tambien puede ser ZM
	 }
	var element =Object.keys(list)[0];
	var result = geoTypes['t'+element.length];
	if (element=='00')
		result = 'NAL';
	if(parseInt(element) >= 9000 && parseInt(element) < 10000)
		result = 'LOC';
	  
	return result;
  },
  getInnerFilterByFilter:function(filter){
	var obj = this;  
	var displayBtn = {
		NAL:['EDO'],
		ZM:['LOC'],
		EDO:['MUN','LOC'],
		MUN:['LOC','AGEB'],
		LOC:['AGEB']
	}
	return displayBtn[filter];  
  },
  validateFilterSelection:function(type,parent){
	var obj = this;
	var list = obj.selectedData.selection.list;
	  
	var current = Object.keys(list)[0];
	//var current = obj.getGeoTypeCurrentSelection();
	var btnList = obj.getGeoTypeByNum(current);
	var currentFilter = obj.selectedData.selection.filter;
	var r = false;
	  
	if(type == 'filter'){ //si lo que ha cambiado es el filtro
		if(btnList.indexOf(currentFilter) >= 0){ //El filtro actual cae dentro de la seleccion actual
			//if(btnList.indexOf(current) >= 0){ //El elemento seleccionado corresponde al filtro
				r = true;
			//}
		}
		if(!r){ //si no es valida la selección actual para el filtro activo, se dispara la selección del elemento geo primero de la lista
			//check obj.options.systemMessage('El corte geografico para la tematización diferia de los elementos geográficos seleccionados, por lo que se seleccionó el primero elemento del listado');		
			setTimeout(function(){
				$('.ecoTool-geoEdo-item:first .ecoTool-geoEdo-icon').click();
			},300);
		}
	}
	if(type == 'geo'){  //si lo que se cambio fue la selección de un elemento geográfico
		if(btnList.indexOf(currentFilter) >= 0){ //El elemento seleccionado corresponde al filtro
			r = true;
		}
		if(!r){ //si no es valida la selección actual para el filtro activo, se dispara la selección del elemento geo primero de la lista
			//check obj.options.systemMessage('Se ajustó el corte geografico de la tematización, acorde a la selección realizada');		
			setTimeout(function(){
				$('.ecoTool-countList-filter-container label.active:first').click();
			},300);
		}
	}
	//dev  
  },
  getGeoTypeByNum:function(id){
	var obj = this;
	var selType = null;
	id = id || '00'; //parent determina el padre geografico que se esta listando
	var cSel = obj.getGeoTypeCurrentSelection();
	if(id == '00'){
		selType = 'NAL';
	}else{
		if(parseInt(id) >= 9000 && parseInt(id)< 10000){
			selType = 'ZM';
		}else{
			var longcve = {'t2':'EDO','t5':'MUN','t9':'LOC','t16':'AGEB'};
			selType = longcve['t'+id.length];
		}
	}
	var displayBtn = {
		NAL:['EDO'],
		ZM:['LOC'],
		EDO:['MUN','LOC'],
		MUN:['LOC','AGEB'],
		LOC:['AGEB']
	}
	return displayBtn[selType];
	//return displayBtn[selType];
  },
  changeGeoType:function(typeGeo){
	  var obj = this;
	  obj.selectedData.geoType = typeGeo;
	  obj.element.attr('geotype',typeGeo);
	  
	  var initValue = {'EDO':'00','ZM':'9000'};
	  
	  obj.printGeoList(initValue[typeGeo]);
  },
  changeColorMap:function(){
	  var obj = this;
	  var data = obj.currentData;
	  var theme = data.theme;
	  var strats = theme.boundaries;
	  var colors = data.colors.colors; 
	  var dataSource = $.extend(true,{},obj.options.dataSource.themeColor);
	  var params = { 
					"id": theme.id, 
					 "variable": data._var.id, 
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
				obj.options.refreshLayer(obj.currentData);	
			}
		});
  },
  getGeoChilds:function(idgeo,func){
	  var obj = this;
	  var dataSource = $.extend(true,{},obj.options.dataSource.geolist);
	  var isZm = (obj.selectedData.geoType == 'ZM'); //determina el tipo de elemento geográfico a solicitar
	  
	  var params = {cvegeo:idgeo,zm:isZm};	
		obj.getData(dataSource,params,function(data){
			if(data.response.success){
				if($.isFunction(func))
					func(data);
			};
		});
  },
  updateUI:function(){
	  var obj = this;
	  var cdata = obj.currentData;
	  
	  //GS
	  var gs = cdata.gs.id;
	  $('.ecoTool-vars-category[selected=selected]').removeAttr('selected');
	  $('.ecoTool-vars-category[idref='+gs+']').attr('selected','selected');
	  
	  obj.refreshVarPanels();
	  
	  //years
	  var year = cdata.yearPos;
	  $('.ecoTool-period-item.selected').removeClass('selected');
	  $('.ecoTool-period-item[idref='+year+']').addClass('selected');
	  
	  //geo
	  var geo = cdata.selection.list;
	  $('.ecoTool-geoEdo-item.selected').each(function(){
		  $(this).removeClass('selected');
	  });
	  for(var x in geo){
		$('.ecoTool-geoEdo-item[idref='+geo[x].id+']').addClass('selected');	  
	  }
	  
	  //panel estratos
	  var method = cdata.method;
	  var strats = cdata.strats;
	  var color = cdata.colors.id;
	  
	  $('.ecoTool-strats-method-item.selected').removeClass('selected');
	  $('.ecoTool-strats-method-item[idref='+method+']').addClass('selected');
	  
	  $('.ecoTool-strats-strat-item.selected').removeClass('selected');
	  $('.ecoTool-strats-strat-item[idref='+strats+']').addClass('selected');
	  
	  $('.ecoTool-strats-colorRamp.selected').removeClass('selected');
	  $('.ecoTool-strats-colorRamp[idref='+color+']').addClass('selected');
	  
	  
	  
  },
  mapTheme:function(){
	  var obj = this;
	  var vals = obj.currentData;
	  var dataSource = $.extend(true,{},obj.options.dataSource.theme);
	  var sector = (vals.ss.id != '')?vals.ss.id:
	  					(vals.s.id != '')?vals.s.id:
							vals.gs.id;
							
	  var numS = vals.strats;
	  
	  var geoSel = vals.selection;
	  var idGeoList = [];
	  for(var x in geoSel.list) //obtiene listado de claves geograficas
	  	idGeoList.push(x)
		
	  var params = {
		  	ent:idGeoList.join(),//vals.geo.id,
		  	sector:parseInt(sector,10),
			variable:vals._var.id,
			estratos:numS,
			year:vals.years[vals.yearPos],
			level:(obj.currentData.geoType == 'EDO')?geoSel.filter.toLowerCase():'zm',
			tipoConsulta:vals.method
		  }	
		obj.getData(dataSource,params,function(data){
			if(data.response.success){
				var vals = data.data;
				obj.currentData.theme = {};
				obj.currentData.valid = true;
				for(var x in vals){
					obj.currentData.theme[x] = vals[x];		
				}
				obj.changeColorMap();
				obj.extentToCvegeo(obj.currentData.geo.id);
				obj.hasChanged = false;
				
				obj.backupData = $.extend({},true,obj.currentData);
				
				if(obj.selectedData.original_strats && obj.selectedData.original_strats != obj.currentData.strats){
					obj.options.systemMessage('Para realizar la tematización, los estratos fueron modificados a '+obj.currentData.strats);		
					//obj.options.systemMessage('Ha cambiado el corte geográfico, se cambiará la selección de subsector o variable económica.');
					obj.updateUI();
				}
				obj.printCurrentValues();
				obj.printThemeDetail();
				//obj.options.refreshLayer(obj.currentData);
			}else{
				var message = data.response.message;
				obj.currentData.valid = false;
				if(message && message == '409'){
					//si se presenta un error disminuye stratos
					if(numS >= 2){
						obj.currentData.strats = obj.currentData.strats-1;
						obj.mapTheme();
					}else{
						if(obj.backupData){
							obj.currentData = $.extend({},true,obj.backupData);	
							obj.selectedData.original_strats = obj.currentData.strats;
							obj.selectedData.method = obj.currentData.method;
							obj.mapTheme();
							obj.updateUI();
						}
					}
				}else{
					if(obj.backupData){
						obj.options.systemMessage('Error al intentar crear el tema de económico, se regresara al tema anterior');
						obj.currentData = $.extend({},true,obj.backupData);	
						obj.selectedData.original_strats = obj.currentData.strats;
						obj.selectedData.method = obj.currentData.method;
						obj.mapTheme();
						obj.updateUI();
					}
				}
			}
			},
			function(){
				obj.currentData.valid = false;
				//si se presenta un error disminuye stratos
				if(numS >= 3){
					obj.currentData.strats = obj.currentData.strats-1;
					obj.mapTheme();
				}else{
					if(obj.backupData){
							obj.currentData = $.extend({},true,obj.backupData);	
							obj.selectedData.original_strats = obj.currentData.strats;
							obj.selectedData.method = obj.currentData.method;
							obj.mapTheme();
							obj.options.systemMessage('Error al intentar crear el tema de económico, se regresara al tema anterior');
					}else{
						obj.options.systemMessage('Error al intentar crear el tema de económico');	
					}	
				}
			});
  },
  	checkPoint:function(pos){
	  var obj = this;
	  var vars  = obj.getCurrentData();
	  var ent = obj.options.getExternalStatus();
	  vars.point = 'POINT('+pos.lon+' '+pos.lat+')';
	  if(!ent)
	  	ent = vars;
	  
	  var cvesgeo = [];
	  for(var x in vars.selection.list)
	  	cvesgeo.push(x);
		
	  var params = {
				level:(vars.geoType == 'ZM')?'zm':vars.selection.filter.toLowerCase(),
				point:'POINT('+pos.lon+' '+pos.lat+')',
				cvegeo:cvesgeo.join()
		  };
	  
	  var dataSource = $.extend(true,{},obj.options.dataSource.infoPoint);
		  //dataSource.url+= '?point=POINT('+pos.lon+' '+pos.lat+')&ent='+ent.geo.id;
	  obj.getData(dataSource,params,function(data){
		   if(data.response.success){
				if(data.data.values){
					obj.options.inGeoArea(vars,data.data.values);
					obj.currentData.lastPoint = data.data.values;				   
				}else{
					obj.options.notInGeoArea(vars,pos);	
				}
		   }
	  });
	  
  },
  getCurrentData:function(pos){
	  var obj = this;
	  var resolution = obj.options.getResolution();
	  var data = obj.currentData;
	  data.var = data._var;
	  data.resolution = resolution;
	  
	  return data;/*{
				gs:data.gs,
				s:data.s,
				ss:data.ss,
				var:data._var,
				geo:data.geo,
				resolution:resolution
			}*/
  },
  createStructure:function(){
	var obj = this;
	var cadena = '';
		if(obj.isCollapsed()){
			cadena+= '<div class="ecoTool-toolBar">';
			cadena+= '	<div class="ecoTool-mainLogo sprite-ecoTool sprite-ecoTool-ecoLogo"></div>';
			cadena+= '	<div class="ecoTool-toolGroup">'+obj.configData.title+obj.currentData.years[obj.currentData.yearPos];
			cadena+= '	</div>';
			cadena+= '</div>';
			cadena+= '<div id="ecoTool_content" class="ecoTool-content" style="display:none">';
			cadena+= '	<div class="ecoTool-content-header" id="ecoTool_content_header"></div>';
			cadena+= '	<div id="ecoTool_mainEdit" title="Configurar" class="ecoTool-mainEdit sprite-ecoTool sprite-ecoTool-pencil"></div>';
			cadena+= '</div>';
		}else{
			cadena+= '<div class="ecoTool-toolBar">';
			cadena+= '	<div class="ecoTool-mainLogo sprite-ecoTool sprite-ecoTool-ecoLogo"></div>';
			cadena+= '	<div class="ecoTool-toolGroup">';
			cadena+= '		<div id="ecoTool_varBtn" val="2" class="ecoTool-toolBtn" selected="selected">';
			cadena+= '			<div title="Seleccione la actividad económica a mostrar en el mapa" class="ecoTool-varBtn sprite-ecoTool sprite-ecoTool-building"></div>';
			cadena+= '			<label>Actividad</label>';
			cadena+= '		</div>';
			cadena+= '		<div id="ecoTool_geoBtn" val="1" class="ecoTool-toolBtn">';
			cadena+= '			<div title="Seleccione la entidad federativa a consultar" class="ecoTool-geoBtn sprite-ecoTool-world"></div>';
			cadena+= '			<label>Geográfico</label>';
			cadena+= '		</div>';
			cadena+= '		<div id="ecoTool_stratBtn" val="3" class="ecoTool-toolBtn">';
			cadena+= '			<div title="Visualice estratos y cambie el color del mapa" class="ecoTool-stratBtn sprite-ecoTool sprite-ecoTool-graph"></div>';
			cadena+= '			<label>Estratos</label>';
			cadena+= '		</div>';
			if(obj.currentData.valid){
				cadena+= '		<div id="ecoTool_infoBtn" val="4" class="ecoTool-toolBtn">';
				cadena+= '			<div title="Visualice la información detallada del tema actual" class="ecoTool-stratBtn sprite-ecoTool-infobig"></div>';
				cadena+= '			<label>Resultado de la consulta</label>';
				cadena+= '		</div>';
			}
			cadena+= '	</div>';
			cadena+= '	<div id="ecoTool_pdfBtn">';
			cadena+= '		<div title="Los Censos Económicos 2014 en el Mapa Digital México" class="sprite-ecoTool sprite-ecoTool-pdf"></div>';
			cadena+= '		<label>Documento metodológico</label>';
			cadena+= '	</div>';
			cadena+= '</div>';
			cadena+= '<div id="ecoTool_content" changed="false" class="ecoTool-content" style="display:none">';
			cadena+= '	<div class="ecoTool-content-header" id="ecoTool_content_header"></div>';
			cadena+= '	<div id="ecoTool_mainAccept" title="Cerrar edición" class="ecoTool-mainEdit sprite-ecoTool-down"></div>';
			//cadena+= '	<div id="ecoTool_mainAcceptActive" title="Aplicar cambios" class="ecoTool-mainEdit sprite-ecoTool sprite-ecoTool-okActive"></div>';
			//cadena+= '	<div id="ecoTool_cancelBtn"><div title="Cancelar edición" class="sprite-ecoTool sprite-ecoTool-cancel"></div></div>';
			cadena+= '	<div id="ecoTool_content_panels">'; 
			cadena+= '		<div id="ecoTool_content_panel1" class="ecoTool-content-panel" value="panel0" style="display:none" filter="CLOSE"></div>';
			cadena+= '		<div id="ecoTool_content_panel2" class="ecoTool-content-panel" value="panel1"></div>';
			cadena+= '		<div id="ecoTool_content_panel3" class="ecoTool-content-panel" value="panel2" style="display:none"></div>';
			cadena+= '		<div id="ecoTool_content_panel4" class="ecoTool-content-panel" value="panel3" style="display:none">Hola</div>';
			cadena+= '	</div>';
			cadena+= '</div>';
		}
		obj.element.html(cadena);
		$('#ecoTool_cancelBtn').click(function(){
			obj.collapsePanel('cancel');
		});
		$('#ecoTool_pdfBtn').click(function(){
			window.open(obj.options.defaultData.pdfPath);
		});	
		$('#ecoTool_content').click(function(){
			if(obj.element.attr('collapsed') == 'true')
				obj.extendPanel();	
		});
		
		$('#ecoTool_mainEdit').click(function(){
			obj.extendPanel();	
		});
		$('.ecoTool-toolBtn').each(function(){
			$(this).click(function(){
				$('.ecoTool-toolBtn[selected=selected]').each(function(){$(this).removeAttr('selected');});
				$(this).attr('selected','selected');
				var panelNum = $(this).attr('val');
				$('.ecoTool-content-panel').each(function(){
					$(this).hide();	
				});
				$('#ecoTool_content_panel'+panelNum).fadeIn();
				if(panelNum == '2'){
					$('#ecoTool_vars_input_search').focus();	
				}
			});
		});
		if(!obj.isCollapsed()){
			$('#ecoTool_mainAccept,#ecoTool_mainAcceptActive').click(function(){
				obj.collapsePanel();
			});
		}
		$('#ecoTool_content').fadeIn();
		obj.printCurrentValues();
		obj.printThemeDetail();
  },
  printThemeDetail:function(){
	  var obj = this;
	  var data = obj.currentData;
	  var theme = data.theme;
	  if(theme){
		  var container = $('#ecoTool_content_panel4');
		  var _var = data._var.label;  
		  var detail = theme.detail;
		  var cadena = '<div class="ecoTool-detail-table-container"><table id="ecoTool_detail_table" class="ecoTool-detail-table" display="estrat_1" cellpadding="4">';
		  	  cadena+= '  <thead><th width="50">Cve. Geo.</th><th  width="250">Nombre</th><th width="60">Valor</th></thead>';
			  cadena+= '<tbody>';
		  var header = '';
		  for (var x in detail){
				var item = detail[x];  
			  	header+= '<div idref="'+(parseInt(x)+1)+'" class="ecoTool-detail-btn-strat '+((x==0)?'selected':'')+'">Estrato '+(parseInt(x)+1);
			  	header+= '	<div class="ecoTool-detail-btn-strat-color" style="background-color:rgb('+((item.rgb).split(' ')).join()+');"></div>';
				header+= '</div>';
				var geos = item.cvegeo;
				for(var y in geos){
					var geo_item = geos[y];
					cadena+= '<tr cvegeo="'+geo_item.cvegeo+'" class="estrat_'+(parseInt(x)+1)+'">';
					cadena+= '	<td width="50">'+geo_item.cvegeo+'</td>';
					cadena+= '	<td width="250">'+geo_item.nombre+'</td>';
					cadena+= '	<td width="60">'+geo_item.indicador+'</td>';
					cadena+= '</tr>';
				}
		  }
			cadena+='</tbody></table></div>';
		 header = '<div class="coTool-detail-strat-container">'+header+'</div>';

		 cadena = header+
		 		  '<div class="ecoTool-detail-title">'+_var+'</div>'+
				  cadena;
		 //Agregado de boton de descargas para estratos
		 cadena+= '<div class="ecoTool-strat-download"><div type="xls" class="sprite-ecoTool sprite-ecoTool-xls ecoTool-strat-download-btn"></div><div type="csv" class="sprite-ecoTool sprite-ecoTool-csv ecoTool-strat-download-btn"></div></div>';
		  
		  container.html(cadena);
		  
		  $('.ecoTool-strat-download-btn').each(function(index, element) {
            	$(this).click(function(){
					var type = $(this).attr('type');
					obj.exportStrats(type);
				});
          });
		  
		  $('.ecoTool-detail-btn-strat').each(function(index, element) {
            	$(this).click(function(){
					var id = $(this).attr('idref');
					$('.ecoTool-detail-btn-strat.selected').removeClass('selected');
					$(this).addClass('selected');
					$('#ecoTool_detail_table').attr('display','estrat_'+id);
				});
          });
		  $('#ecoTool_detail_table tbody tr').each(function(index, element) {
          		$(this).click(function(){
					obj.extentToCveGeo($(this).attr('cvegeo'));		
				});  
          });
		  
		  
	  }
  },
  getGsVar:function(id){
	  var obj = this;
	  var list = obj.options.var_gs;
	  var r = null;
	  for(var x in list){
			if(list[x].id == id)
				r = list[x];
	  }
	  return r;
  },
  getGeoById:function(id){
	  var obj = this;
	  var list = obj.data.geoList;
	  var r = null;
	  for(var x in list){
			var item = list[x];
			if(item.cvegeo == id){
				r = {id:item.cvegeo,label:item.nombre};
				break;
			}  
	  }
	  return r;
  },
  changeFilter:function(text){ 
			 var obj = this;
			 var id = 'ecoTool_geo_filter_text';
			 text = text || $('#'+id).val();
			 text = text.toLowerCase();
			 if(text == ''){
				$('.ecoTool-geoEdo-item').each(function(index, element) {
					$(this).css({'display':'block'});
			 	});	 
			 }else{
				$('.ecoTool-geoEdo-item').each(function(index, element) {
					$(this).css({'display':'none'});
			 	});	 
				$('.ecoTool-geoEdo-item[label*='+text+']').each(function(index, element) {
					$(this).css({'display':'block'});
			 	});	 	 
			 }
  },
  setInputFilter:function(){
			 	var obj = this;
			 	var id = 'ecoTool_geo_filter_text';
				var callback = obj.changeFilter;
				$('#'+id).bind("keypress", function(evt){
				var otherresult = 12;
				if(window.event != undefined){
					otherresult = window.event.keyCode;
				}
				var charCode = (evt.which) ? evt.which : otherresult;  
				if(charCode == 13 || charCode == 12){
					if (charCode==13)/*$("#"+idClick).click();*/
					if (charCode ==12 && evt.keyCode == 27){  //atrapa esc y limpia
						setTimeout(function(){$('#'+id).val('');},200);
					}
					if(charCode == 46 && $.isFunction(callback)){
						setTimeout(function(){callback('');},200);
					}
					if(charCode == 27){
					   setTimeout(function(){$('#'+id).val(''); callback('');},200);
					}
				}else{
					var keyChar = String.fromCharCode(charCode);
					var keyChar2 = keyChar.toLowerCase();
					var re =   /[\u00E0-\u00E6\u00E8-\u00EB\u00EC-\u00EF\u00F2-\u00F6\u00F9-\u00FC\u0061-\u007a\u00f10-9 \-\,\.]/
					var result = re.test(keyChar2);
					result = (charCode == 8)?true:result;
					if (result){
						setTimeout(function(){callback(obj.stringAsType($('#'+id).val()));},200);
						
					}
					return result; 
					
				}
												
				}).keydown(function(e){
				if(((e.which == 46) || (e.which == 8)) && $.isFunction(callback)){
					setTimeout(function(){callback(obj.stringAsType($('#'+id).val()));},200);
				}
					if(e.which == 27){
					  setTimeout(function(){$('#'+id).val(''); callback('');},200);
					}
				});	 
  },
  extentToCveGeo:function(cvegeo){
		var obj = this;
		var dataSource = $.extend(true,{},obj.options.dataSource.getExtent);
			dataSource.url+=cvegeo;
		var params = {};	
		obj.getData(dataSource,params,function(data){
			if(data.response.success){
				obj.options.map.goCoords(data.data.extent);
			};
		});
  },
  geoListAdd:function(item){
	  var obj = this;
	  var selection = obj.selectedData.selection;
	  item.geoType = obj.selectedData.geoType;
	  var counTypes = 0;
	  
	  var itemType = 't'+item.id.length;
	  var geoType = selection
	  //refresca los tipos que existen
	  selection.types = {}
	  var countElements = 0;
  	  
	  if(obj.selectedData.geoType == 'EDO'){
		  if(item.id != '00'){ //si se agrega un elemento diferente a nacional elimina a nacional
			delete selection.list['00'];
		  }else{
			  selection.list = {};
		  }
	  }else{
		  if(item.id != '9000'){ //si se agrega un elemento diferente a nacional elimina a nacional
			delete selection.list['9000'];
		  }else{
			  selection.list = {};
		  }
	  }
	  
	  var geoType = '';
	  for(var x in selection.list){
			selection.types['t'+(selection.list[x].id.length)] = true;  
		  	selection.geoType = selection.list[x].geoType;
			countElements++;	
	  }
	  
	  if(!selection.types[itemType] || (selection.geoType != '' && selection.geoType != item.geoType)){  //si el elemento seleccionado es de tipo distinto a la selección actual limpia y reinicia selección
		  selection.list = [];
		  selection.types = {};
		  countElements = 0;
	  }
	  
	  if(countElements < obj.options.maxGeo){
		  //obtiene cuantos tipos diferentes existen
		  for(var x in selection.types){
			counTypes++;
		  }
		  //valido solo si existen mas elementos agregados del mismo tipo 0 no hay selección, ademas de que ya haya sido agregado el elemento
		  if((selection.types['t'+(item.id.length)] || counTypes <= 1) &&  !selection.list[item.id]){
			selection.types['t'+(item.id.length)] = true;
			selection.list[item.id] = item;
			return true;
		  }else{
			return false;
		  }
	  }
  },
  existGeoSelected:function(id){
	 var obj = this;
	 var selection = obj.selectedData.selection;
	 if(selection.list[id]){
		return true	 
	 }else{
		return false;	 
	 }
  },
  geoListRem:function(item){
	  var obj = this;
	  var selection = obj.selectedData.selection;
	  
	  //elimina el elemto del listado
	  delete selection.list[item.id];
	  
	  //refresca los tipos que existen
	  selection.types = {}
	  for(var x in selection.list){
		selection.types['t'+(selection.list[x].id.length)] = true;  
	  }
	  //cuenta elementos en lista
	  var countElements = 0;
	  for(var x in selection.list){
		countElements++;
	  }
	  //si no hay elementos regresa a nacional
	  if(!countElements){
		selection.list['00'] = {id:'00',label:'Nacional'};  
	  }
	  
	  
  },
  printGeoListSelection:function(parent){
	 var obj = this;
	 var list = obj.selectedData.selection.list;
	 var cadena = '';
	 //elimina todo los elementos seleccionados
	 $('.ecoTool-geoEdo-item[selected=selected]').removeAttr('selected');
	 
	 var geoTypes = {
			t2:'EDO',
		 	t5:'MUN',
		 	t9:'LOC',
		    t13:'AGEB'//tambien puede ser ZM
	 } 
	 for(var x in list){
		 var item = list[x];
		cadena+= '<div class="geoList-selected-item"><label>'+item.label+'</label><div idref="'+item.id+'" class="geoList-remove-btn sprite-ecoTool sprite-ecoTool-close"></div></div>';
		$('.ecoTool-geoEdo-item[idref='+item.id+']').attr('selected','selected');	
	 }
	 
	 $('#ecoTool_edolist_selection_list').html(cadena);
	 $('.geoList-remove-btn').each(function(index, element) {
        $(this).click(function(){
			idref = $(this).attr('idref');
			obj.geoListRem(obj.getGeoById(idref));
			obj.printGeoListSelection();
		})
    });
	
	//imprimir control de filtros
	var cadena = '';
	var countList = 0;
	for(var x in list){
		countList++;
	}
	
	cadena+='<div id="ecoTool_countList_selection" class="ecoTool-countList-selection"><label>'+countList+'</label><div class="ui-icon  ui-icon-triangle-1-s"></div></div>';
	cadena+='<div class="ecoTool-countList-filter-container"><div class="ecoTool-filter-container-header">Tematizar por:</div>';

	obj.selectedData.selection.filter = (!obj.selectedData.selection.filter)?'EDO':obj.selectedData.selection.filter;
	
	  
	var current = element =Object.keys(list)[0];
	var btnList = obj.getGeoTypeByNum(current);
	var cType = obj.getGeoTypeCurrentSelection(); //Current Type Selection
	  
	  
	var currentFilter = obj.selectedData.selection.filter;
	  //botones de selección de typo geográfico  
	cadena+= '<label class="ecoTool-btn-filter '+((btnList.indexOf('EDO') >= 0)?'active':'inactive')+' '+((currentFilter == 'EDO')?'selected':'')+'" idref="EDO">EDO</label>';
	cadena+= '<label class="ecoTool-btn-filter '+((btnList.indexOf('MUN') >= 0)?'active':'inactive')+' '+((currentFilter == 'MUN')?'selected':'')+'" idref="MUN">MUN</label>';
	//Ajuste temporal
	//cadena+= '<label class="ecoTool-btn-filter '+((btnList.indexOf('LOC') >= 0)?'active':'inactive')+' '+((currentFilter == 'LOC')?'selected':'')+'" idref="LOC">LOC</label>';	
	//cadena+= '<label class="ecoTool-btn-filter '+((btnList.indexOf('AGEB') >= 0)?'active':'inactive')+' '+((currentFilter == 'AGEB')?'selected':'')+'" idref="AGEB">AGEB</label>';
	  
	cadena+= '</div>';
	
	$('#ecoTool_edolist_selection_header').html(cadena);
	$('#ecoTool_countList_selection').click(function(){
		var filter = $('#ecoTool_content_panel1').attr('filter');
		filter = (!filter)?'OPEN':(filter=='CLOSE')?'OPEN':'CLOSE';
		$('#ecoTool_content_panel1').attr('filter',filter);
	});
	$('.ecoTool-btn-filter.active').each(function(index, element) {
        $(this).click(function(e){
			obj.selectedData.selection.filter = $(this).attr('idref');
			obj.validateFilterSelection('filter',parent);
			
			$('.ecoTool-btn-filter.selected').removeClass('selected');
			obj.setChanged();
			if($(this).attr('idref') == 'AGEB'){
				$('.ecoTool-vars-category[idref=0]').click();
			}
			$('.ecoTool-vars-tool-container').attr('geotype', $(this).attr('idref'));
			$(this).addClass('selected');
		});
    });
	 
  },
  printGeoList:function(idgeo,notNav){ //se usa para indicar que no esta navegando para buscar elementos
	 var obj = this;
	 var l_idgeo = idgeo.length;
	 var longcve = [2,5,9,16];
	 var t_idgeo = idgeo;
	 if(notNav){
		var t_pos = longcve.indexOf(l_idgeo);
	 	t_idgeo = (t_pos <= 0)?'00':idgeo.substr(0,longcve[t_pos-1]);
  	 }
	 var selectedData = obj.selectedData;
	 var geonames = (obj.selectedData.geoType == 'EDO')?['Estados','Municipios','Localidades','Agebs']:['ZM']; 
	 var panel1 = $('#ecoTool_edolist_container');
	 obj.getGeoChilds(t_idgeo,function(data){
		 	data = data.data;
			var list = data[data.Tipo];

			var pos = geonames.indexOf(data.Tipo);
			var long = longcve[pos];
			var t_cve = list[0].cvegeo;
			
			var parent = (pos <= 1)?
					(obj.selectedData.geoType == 'EDO')?'00':'9000'		
					:t_cve.substr(0,longcve[pos-2]);
						
			if(pos > 0)
				$('#ecoTool_content_panel1').addClass('childlist');
			else
				$('#ecoTool_content_panel1').removeClass('childlist');
			
			obj.data.lastcvegeo = parent; 
			
			obj.data.geoList = list;
			var cadena = '';
			
			//Agrega Nacional si son estados
			if(pos == 0){
				if(obj.selectedData.geoType == 'EDO'){
					list.unshift({cvegeo: "00", nombre: "Nacional"});	
				}else{
					list.unshift({cvegeo: "9000", nombre: "Todas las zonas metropolitanas"});	
				}
			}
			
			for(var x in list){
					var item = list[x];
					var isSelected = (item.cvegeo == selectedData.geo.id);
					cadena+= '<div class="ecoTool-geoEdo-item" label="'+obj.stringAsType(item.nombre.toLowerCase())+'" idparent="'+parent+'" idref="'+item.cvegeo+'" '+((isSelected)?'selected="selected"':'')+'>';
					cadena+= '	<div class="ecoTool-geoEdo-icon">';
					cadena+= '		<div class="ecoTool-geoEdo-icon-sel sprite-ecoTool sprite-ecoTool-itemSelected"></div>';
					cadena+= '		<div class="ecoTool-geoEdo-icon-unsel sprite-ecoTool sprite-ecoTool-itemUnselected"></div>';
					cadena+= '	</div>';
					cadena+= '	<div class="ecoTool-geoEdo-item-label">'+item.nombre+'</div>';
					//comentado VER MAS...
					if(item.childs){ //preenta icono de avanzar solo cuando llega hasta nivel de municipio
						cadena+= '	<div idref="'+item.cvegeo+'" class="ecoTool-geo-seemore">';
						cadena+= '		<div class="ecoTool-geoEdo-icon-seemore sprite-ecoTool sprite-ecoTool-seemore"></div>';
						cadena+= '	</div>';
					}
					cadena+= '</div>';
			 }
			 panel1.html(cadena);
				  $('.ecoTool-geo-seemore').each(function(){
						$(this).click(function(e){
							var idgeo = $(this).attr('idref');
							obj.printGeoList(idgeo);
							e.stopPropagation();
						})
				  });
				  $('.ecoTool-geoEdo-icon').each(function(){
						$(this).click(function(e){
							var elem = $(this).parent();
							var idref = elem.attr('idref');
							if(!obj.existGeoSelected(idref)){
								selectedData.geo = obj.getGeoById(idref);
								if(obj.geoListAdd(obj.getGeoById(idref))) //agrega elemento, si exito al agregarlo procede a impresion de elementos seleccionados
									obj.printGeoListSelection(idgeo);
									
								/*$('.ecoTool-geoEdo-item[selected=selected]').removeAttr('selected');
								elem.attr('selected','selected');*/
								obj.extentToCveGeo(idref);
								
								obj.validateFilterSelection('geo',idgeo); //valida la selección actual
								
							}else{
								obj.geoListRem(obj.getGeoById(idref));
								obj.printGeoListSelection(idgeo);
							}
							obj.setChanged()
							obj.refreshVarPanels();
							e.stopPropagation();
						});
				  });
				  
				  
				  $('.ecoTool-geoEdo-item').each(function(){
						$(this).click(function(){
							if($(this).attr('selected') === undefined){
								var idref = $(this).attr('idref');
								/*selectedData.geo = obj.getGeoById(idref);
								$('.ecoTool-geoEdo-item[selected=selected]').removeAttr('selected');
								$(this).attr('selected','selected');*/
								obj.extentToCveGeo(idref);
								/*obj.setChanged()
								obj.refreshVarPanels();*/
							}
					})
			  });
			  
			  obj.printGeoListSelection(idgeo);
	 });
  },
  printPanels:function(){
	  var obj = this;
	  var panel1 = $('#ecoTool_content_panel1');
	  var panel2 = $('#ecoTool_content_panel2');
	  var panel3 = $('#ecoTool_content_panel3');
	  var edos = obj.configData.edos;
	  var selectedData = obj.selectedData;
	  //Panel 01 -------------------------------------------------------
	  var cadena = '<div id="ecoTool_edolist_back_container" class="ecoTool-edolist-back-container ecoTool-animated">';
	  	  cadena+= '	<div class="ecoTool-geoEdo-icon-back sprite-ecoTool sprite-ecoTool-back"></div>';
		  cadena+= '</div>';	
		  cadena+= '<div id="ecoTool_edolist_header" class="ecoTool-edolist-header">';
		  cadena+= '	<div class="ecoTool-typeGeo"><div idref="EDO" class="ecoTool-typeGeoBtn '+((selectedData.geoType=='EDO')?'selected':'')+'" title="Selección por Estados">EDOS</div>';
	  	  //Ajuste temporal
	  	  cadena+=			'<div idref="ZM" style="display:none" class="ecoTool-typeGeoBtn '+((selectedData.geoType=='ZM')?'selected':'')+'" title="Selección por Zonas Metropolitanas">ZM</div></div>';
		  cadena+= '	<input id="ecoTool_geo_filter_text" type="text" placeholder="filtrar" >';
		  cadena+= '</div>';
		  cadena+= '<div id="ecoTool_edolist_container" class="ecoTool-edolist-container ecoTool-animated"></div>';
		  cadena+= '<div id="ecoTool_edolist_selection_container" class="ecoTool-edolist-selection-container ecoTool-animated">';
		  cadena+= '	<div id="ecoTool_edolist_selection_header" class="ecoTool-edolist-selection-header ecoTool-animated"></div>';
		  cadena+= '	<div id="ecoTool_edolist_selection_list" class="ecoTool-edolist-selection-list ecoTool-animated"></div>';
		  cadena+= '</div>';
	  panel1.html(cadena);
	  
	  $('#ecoTool_edolist_back_container').click(function(){
			obj.printGeoList(obj.data.lastcvegeo);  //no es navegacion
	  });
	  
	  $('.ecoTool-typeGeoBtn').each(function(){
		 $(this).click(function(e){
			 var idref = $(this).attr('idref');
			 $('.ecoTool-typeGeoBtn.selected').removeClass('selected');
			 $(this).addClass('selected');
			 obj.changeGeoType(idref);
			 e.stopPropagation();
		 }) 
	  });
	  
	  
	  obj.printGeoList(selectedData.geo.id,true);
	  obj.printGeoListSelection();
	  obj.setInputFilter();
	 //Panel 02-------------------------------------------------------------
	  var gs = this.options.var_gs;
	  cadena = '<div class="ecoTool-vars-tool-container">';
	  for(var x in gs){
		var item = gs[x];
		cadena+= '<div '+((item.id == obj.currentData.gs.id)?'selected="selected"':'')+'val="'+(parseInt(x,10)+1)+'" idref="'+item.id+'" _name="'+item.name+'" class="ecoTool-vars-category">';
		cadena+= 	'<div class="'+((item.sprite)?item.sprite:'')+'" title="'+item.title+'" ></div>';
		cadena+= 	((!item.sprite)?'<label title="'+item.title+'" >'+item.name+'</label>':'<label title="'+item.title+'" >'+item.label+'</label>');
		cadena+= '</div>';
	  }
	  cadena+= '</div>';
	  cadena+= '<div class="ecoTool-vars-panel-container">';
	  cadena+= '	<div id="ecoTool_vars_panel1" class="ecoTool-vars-panel" ></div>';
	  cadena+= '	<div id="ecoTool_vars_panel2" class="ecoTool-vars-panel" style="display:none"></div>';
	  cadena+= '	<div id="ecoTool_vars_panel3" class="ecoTool-vars-panel" style="display:none"></div>';
	  cadena+= '</div>';
	  panel2.html(cadena);
	  $('.ecoTool-vars-category').each(function(index, element) {
        		$(this).click(function(e) {
					$('#ecoTool_block_ssector').remove();
                    $('.ecoTool-vars-category[selected=selected]').removeAttr('selected');
					$(this).attr('selected','selected');
					var gs = obj.getGsVar($(this).attr('idref'));
					obj.selectedData.gs = {id:gs.id,label:gs.name,title:gs.label};
					obj.selectedData.s = {id:'',label:''};
					obj.selectedData.ss = {id:'',label:''};
					obj.selectedData.total = '';
					obj.setChanged();
					if(gs.id == 0){
						$('#ecoTool_vars_panel1').html('');
					}
					
					obj.printCurrentValues();
					obj.refreshVarPanels();
					//obj.printVarPanels({id:$(this).attr('idref'),name:$(this).attr('_name')});
                });
		});
	  //Panel03 ------------------------------------------------------------

	   var minStrats = obj.options.config.numStrats.min;
	   var maxStrats = obj.options.config.numStrats.max;
	   var methods = obj.options.config.methods;
	  
	   var colorRamps = obj.configData.colorRamps;
	   var currentRamp = obj.currentData.colors;
	   
	   
	   cadena = '<div class="ecoTool-strats-container">';
	   cadena+= '	<div class="ecoTool-strats-currentRamp-container">';
	   cadena+= '		<div class="ecoTool-strats-transparency-container">';
	   cadena+= '			<div class="ecoTool-strats-transparency-title">Transparencia</div>';
	   cadena+= '			<div id="ecoTool_strats_trasparencyControl" class="ecoTool-strats-transparency-tool"></div>';
	   cadena+= '		</div>';
	   cadena+= '		<div id="ecoTool_strats_data_config" class="ecoTool-strats-data-config">';
	   
	   cadena+= '			<div class="ecoTool-strats-data-config-methods-title">Métodos de estratificación</div>';
	   
	   cadena+= '			<div id="ecoTool_strats_data_config_methods" class="ecoTool-strats-data-config-methods">';
						   for(var x in methods){
								var item = methods[x];
								cadena+= '<div idref="'+item.id+'" title="'+item.title+'" class="ecoTool-strats-method-item '+((item.id == obj.selectedData.method)?'selected':'')+'">'+item.name+'</div>';
						   }
	   cadena+= '			</div>';
	   
	   cadena+= '			<div class="ecoTool-strats-strat-title">No.Estratos</div>';
	   cadena+= '			<div id="ecoTool_strats_data_config_strats" class="ecoTool-strats-data-config-strats">';
						   for(var x = minStrats; x <= maxStrats;x++){
								cadena+= '<div idref="'+x+'" class="ecoTool-strats-strat-item '+((x == obj.selectedData.strats)?'selected':'')+'">'+x+'</div>';
						   }
	   cadena+= '			</div>';
	   cadena+= '		</div>';
	   cadena+= '		<div id="ecoTool_strats_data_container" class="ecoTool-strats-data-container"></div>';
	   cadena+= '	</div>';
	   cadena+= '	<div id="ecoTool_strats_currentRamps_container" class="ecoTool-strats-currentRamps-container"></div>';
	   cadena+= '</div>';
	   panel3.html(cadena);
	   
	   obj.printThemeStats();
	   $('.ecoTool-strats-method-item').each(function(index, element) {
        	$(this).click(function(e){
				$('.ecoTool-strats-method-item.selected').removeClass('selected');
				$(this).addClass('selected');
				obj.selectedData.method = $(this).attr('idref');
				obj.setChanged();
			});
    	});
		$('.ecoTool-strats-strat-item').each(function(index, element) {
        	$(this).click(function(e){
				$('.ecoTool-strats-strat-item.selected').removeClass('selected');
				$(this).addClass('selected');
				obj.selectedData.strats = parseInt($(this).attr('idref'),10);
				obj.selectedData.original_strats = obj.selectedData.strats;
				obj.setChanged();
			});
    	});
	   
	   
	   $( "#ecoTool_strats_trasparencyControl" ).slider({
		  range: "max",
		  min: 1,
		  max: 100,
		  value: obj.configData.transparency,
		  slide: function( event, ui ) {
			obj.currentData.transparency = ui.value;
			obj.configData.transparency = ui.value;
			obj.options.onTransparency(ui.value);
		  }
		});
	   
	   cadena = '';
	   for(var x in colorRamps){
			var ramp = colorRamps[x];   
			cadena+= obj.createRampColor(ramp); 
	   }
	   $('#ecoTool_strats_currentRamps_container').html(cadena);
	   obj.printThemeStats();
	   
	   $('.ecoTool-strats-colorRamp').each(function(){
			$(this).click(function(){
				
				$('.ecoTool-strats-colorRamp[selected=selected]').each(function(index, element) {
						$(this).removeAttr('selected');
				});
				$(this).attr('selected','selected');
				var idref = $(this).attr('idref');
	   			obj.rollbackColor = $.extend(true,{},obj.selectedData.colors);
				obj.selectedData.colors = obj.configData.colorRamps[parseInt(idref,10)];
				obj.currentData.colors = selectedData.colors;
				obj.changeColorMap();
				obj.printThemeStats();
			})   
	   })
	   
	   
  },
  createRampColor:function(ramp){
	  	var obj = this;
		var colorRamps = obj.configData.colorRamps;
		var currentRamp = obj.currentData.colors;
		var colors = ramp.colors;
		var snum = obj.options.stratCount;
		var cadena= '<div idref="'+ramp.id+'" class="ecoTool-strats-colorRamp" '+((currentRamp.id == ramp.id)?'selected="selected"':'')+'>';
			for(var x in colors){
				var width = 100/snum;
				cadena+='<div class="ecoTool-strats-ramp-color" style="background-color:'+colors[x]+';width:'+width+'%"></div>';	
			}
			cadena+='</div>';
		return cadena; 
  },
  printThemeStats:function(){
	 var obj = this;
	 var colorRamps = obj.configData.colorRamps;
	 var data = obj.currentData.theme;
	 var currentRamp = obj.currentData.colors;
	 var noDataStrat = obj.options.noDataStrat;
	 var strats = $.extend(true,[],data.boundaries).concat(noDataStrat);

	 var cadena = '<div class="ecoTool-theme-strat-info">';
	 	 var leftStr = '';
		 var rightStr = '';
		 var med = ((strats.length / 2) > (2 % strats.length))?(2 % strats.length)+1:(2 % strats.length);
		 for(var x in strats){
			var item = strats[x];
			var style = item.style;
			
			if(!style){
				var color = currentRamp.colors[x];
				var _cadena = '<div style="float:'+((parseInt(x,10) < med)?'left':'right')+'" class="ecoTool-theme-strat-info-item"><span style="background-color:'+color+'"/>';
					_cadena+= '<label>Estrato: <b>'+item.stratum+'</b></label><label>Frecuencia: <b>'+item.n+'</b></label></div>';		 
			}else{
				var _cadena = '<div style="float:'+((parseInt(x,10) < med)?'left':'right')+'" class="ecoTool-theme-strat-info-item"><span style="'+style+'"/>';
					_cadena+=  '<label style="padding-top:6px;">'+item.label+'</label></div>';		 		
			}

			if((parseInt(x,10) <= med)){
				leftStr+=_cadena;
			}else{
				rightStr+=_cadena;
			}
		 }
		 cadena+= '		<div class="ecoTool-theme-strat-left-info">'+leftStr+'</div>';
		 cadena+= '		<div class="ecoTool-theme-strat-right-info">'+rightStr+'</div>';
	 	 cadena+= '</div>';
	   
	 	 cadena+= '<div class="ecoTool-theme-normalInfo ecoTool-theme-info">';
		 cadena+= '		<div><b>Media:</b><label>'+obj.formatMoney(data.mean)+'<label></div>';
		 cadena+= '		<div><b>Mediana:</b><label>'+obj.formatMoney(data.median)+'<label></div>';
		 cadena+= '		<div><b>Moda:</b><label>'+obj.formatMoney(data.mode)+'<label></div>';
	 	 cadena+= '</div>';
		 
		 cadena+= '<div class="ecoTool-theme-mainInfo ecoTool-theme-info">';
	 	 cadena+= '		<div><b>Indicador:</b><label>'+obj.formatMoney(data.indicator)+'<label></div>';
		 cadena+= '		<div><b>Elementos:</b><label>'+obj.formatMoney(data.n)+'</label></div>';
		 cadena+= '		<div><b>D.Estd:</b><label>'+obj.formatMoney(data.sd)+'<label></div>';
	 	 cadena+= '</div>';
	
	$('#ecoTool_strats_data_container').html(cadena);
	 
  },
  adjustBlockSize:function(PO){ //Panel Open
  	var obj = this;
	
	var SSBan = !($('#ecoTool_block_ssector').attr('id') === undefined);
	var SBan = !($('#ecoTool_block_sector').attr('id') === undefined);
	
	var _s = $('#ecoTool_block_sector');
	var _ss = $('#ecoTool_block_ssector');
	var _var = $('#ecoTool_block_vars');
	
	
	$('.ecoTool-block').each(function(){
		$(this).removeClass('ecoTool-block-selected')	
	});
	
	switch (PO){
		case 's':
			if(SSBan){
				_s.css({bottom:'62px',height:''});
				_ss.css({top:'',bottom:'92px',height:'25px'});
			}else{
				_s.css({bottom:'95px',height:''});	
			}
			_var.css({top:'',bottom:'62px',height:'25px'});
		break;
		case 'ss':
				_s.css({bottom:'',height:'25px'});
				_ss.css({top:'31px',bottom:'',height:'',bottom:'95px'});
				_var.css({top:'',bottom:'62px',height:'25px'});
			
		break;
		case 'var':
			if(!SSBan && !SBan){
				_var.css({top:'0px',bottom:'62px',height:''});
			}else{
				if(SSBan){
					_s.css({bottom:'',height:'25px'});
					_ss.css({top:'31px',bottom:'',height:'25px'});
					_var.css({top:'62px',bottom:'62px',height:''});
				}else{
					_s.css({bottom:'',height:'25px'});
					_var.css({top:'31px',bottom:'62px',height:''});
				}
			}
		break;
	}
  },
  getVar:function(id){
	  var obj = this;
	  var vars = obj.options.var_vals;
	  vars = vars.var_eco.list.concat(vars.var_relan.list);
	  var r = null;
	  for(var x in vars){
		  var _var = vars[x];
		  if(_var.id == id){
			  r = _var;
			  break;
		  }
	  }
	  return r;
  },
  getSector:function(id){
	  var obj = this;
	  var r = null;
	  var sector = obj.serviceData.sector;
	  var ssector = obj.serviceData.ssector;
	  
	  for(var x in sector){
		  var item = sector[x];
			if(item.id == id){
				r = item;
				r.label = r.name;
				break;
			}  
	  }
	  if(!r)
	  for(var x in ssector){
		  var item = ssector[x];
			if(item.id == id){
				r = item;
				r.label = r.name;
				break;
			}  
	  }
	  return r;
  },
  getEcoVar:function(id){
	var obj = this;
	var vars = obj.options.var_vals;
	var r = null;
	for(var list in vars){
		var items = vars[list].list
		for(var x in items){
			var item = items[x];
			if(item.id == id){
				r = item;
				r.type = list;	
				break;
			}
		}
	}
	return r;
  },
  getRule:function(){
	var obj = this;  
	var data = obj.selectedData;
	
	var isNal = (parseInt(data.geo.id,10) == 0);
	var isGs = data.gs.id != '' || data.gs.id == 0;
	var isS =  data.s.id != '';
	
	return{
		showSS:((isNal)?true:(isGs && !isS)?true:false),
		showVE:((isNal)?true:(isGs && !isS)?true:false)
	}
	
  },
  refreshVarPanels:function(ban){
	var obj = this;
	var data = obj.selectedData;
	
	if(!obj.getRule().showSS){
		data.ss = {id: '',label: ''};
		obj.adjustBlockSize('s');
		$('#ecoTool_block_ssector').remove();
	}
	if(!obj.getRule().showVE){
		var tVar = obj.getEcoVar(data._var.id);
		if(tVar.type == 'var_eco'){
			var firstVar = obj.options.var_vals.var_relan.list[0];
			data._var = firstVar;
			obj.options.systemMessage('Se ha cambiado la relación analítica por '+firstVar.label);	
			obj.printVarEco(true);
		}
	}
	if(!ban){
		//obj.printVarPanels({id:data.gs.id,name:data.gs.name});
		var tabSel = $('.ecoTool-vars-category[selected=selected]');
		obj.printVarPanels({id:tabSel.attr('idref'),name:tabSel.attr('_name')},function(){
			if(obj.selectedData.s.id != '' && obj.getRule().showSS){
				obj.printVarPanelsSub(obj.selectedData.s.id);
			}
			obj.printVarEco(true);
		});
	}
		
	obj.printCurrentValues();
  },
  printVarEco:function(_refresh){
	obj = this; 
	var gs = obj.options.var_gs;
	var sdata = obj.selectedData; 
	var cadena = '';
	var vals = obj.options.var_vals.var_eco.list;
	
	if(!_refresh){
		cadena+='<div id="ecoTool_block_vars" class="ecoTool-block ecoTool-block-vars">';
		cadena+='	<div title="Seleccione indicador a consultar" type="var" class="ecoTool-var-title ecoTool-block-title">Información Económica';
		cadena+='	</div>';
		cadena+='	<div id="ecoTool_vars_container" class="ecoTool-vars-container">';
	}
	
	//solo en nacional o si no ha seleccionado un sector
	if(obj.getRule().showVE){
		cadena+='		<div class="ecoTool-vars-section-title">'+obj.options.var_vals.var_eco.label+'</div>';
		for(var x in vals){
			var val = vals[x];
			cadena+='<div '+((val.id == sdata._var.id)?'selected="selected"':'')+' idref="'+val.id+'" type="ECO" class="ecoTool-block-item ecoTool-var-item">';
			cadena+='	<div idref="'+val.id+'" class="ecoTool-var-info sprite-ecoTool sprite-ecoTool-info"></div><label>'+val.label+'</label>';
			cadena+='</div>';	
		}
	}
	
	var vals = obj.options.var_vals.var_relan.list;
	cadena+='	<div class="ecoTool-vars-section-title">'+obj.options.var_vals.var_relan.label+'</div>';
	for(var x in vals){
		var val = vals[x];
		cadena+='<div '+((val.id == sdata._var.id)?'selected="selected"':'')+' idref="'+val.id+'" type="REL" class="ecoTool-block-item ecoTool-var-item">';
		cadena+='	<div idref="'+val.id+'" class="ecoTool-var-info sprite-ecoTool sprite-ecoTool-info"></div><label>'+val.label+'</label>';
		cadena+='</div>';	
	}
	if(!_refresh){
		cadena+='	</div>';
		
		cadena+='</div>';
		
		$('#ecoTool_vars_panel1').append(cadena);	
		obj.adjustBlockSize('var');	
	}else{
		$('#ecoTool_vars_container').html(cadena);	
		obj.adjustBlockSize('var');
	}
	
	$('.ecoTool-block-title').each(function(){
		$(this).click(function(){
			obj.adjustBlockSize($(this).attr('type'));
		});
	});
	
	$('.ecoTool-var-info').each(function(index, element) {
		$(this).click(function(){
			obj.openDialogVar($(this).attr('idref'));		
		})
	});
	$('.ecoTool-var-item').each(function(index, element) {
		$(this).click(function(){
			var idref = $(this).attr('idref');
			obj.selectedData._var = obj.getVar(idref);
			var areAll = ($('.ecoTool-vars-category[selected=selected]').attr('idref') == '0');
			if(areAll){
				var gs = $('.ecoTool-vars-category[selected=selected]');
				gs = obj.getGsVar(gs.attr('idref'));
				
				obj.selectedData.gs = {id:gs.id,label:gs.name,title:gs.label};
				obj.selectedData.s = {id:'',label:''};
				obj.selectedData.ss = {id:'',label:''};
				
				obj.selectedData.total = '';
			}
			obj.setChanged();
			obj.printCurrentValues();
		});
	});
  },
  printVarPanels:function(sec,func){
		var obj = this;
		var gs = obj.options.var_gs;
		var sdata = obj.selectedData;
		//funcion para impresion general;
		var dataSource = $.extend(true,{},obj.options.dataSource.sectors);
			dataSource.url+= '/'+sec.id;
		if(sdata.gs.id != 0 || sdata.gs.id != '0'){
			obj.getData(dataSource,{},function(data){
				var cadena = '';
				if(data.response.success){
					var sectors = data.data.sectors;
					
						obj.serviceData.sector = $.extend(true,[],data.data.sectors);
						//impresion de sectores del Gran Sector
						cadena+='<div id="ecoTool_block_sector" class="ecoTool-block ecoTool-block-sector" idref="'+sec.id+'">';
						cadena+='	<div type="s" title="Filtre por sector" class="ecoTool-sector-title ecoTool-block-title">Sectores';
						cadena+='	</div>';
						cadena+='	<div class="ecoTool-sectors-container">';
						for(var x in sectors){
							var sector = sectors[x];
							cadena+='<div '+((sector.id == sdata.s.id)?'selected="selected"':'')+' idref="'+sector.id+'" type="'+sector.type+'" class="ecoTool-block-item ecoTool-sector-item" subsector="'+(sector.count > 0)+'">';
							cadena+='	<div>'+sector.name;
							cadena+='	</div>';
							cadena+='</div>';
						}
						cadena+='	</div>';
						cadena+='</div>';
						
						$('#ecoTool_vars_panel1').html(cadena);
						obj.printVarEco();
						obj.printPeriodSelection();
						
						obj.adjustBlockSize('s');
						$('.ecoTool-sector-item').each(function(){
							$(this).click(function(e){
								console.log('click');
								var idref = $(this).attr('idref');
								obj.selectedData.s = obj.getSector(idref);
								var gs = $('.ecoTool-vars-category[selected=selected]');
								gs = obj.getGsVar(gs.attr('idref'));
								obj.selectedData.gs = {id:gs.id,label:gs.name,title:gs.label};
								obj.selectedData.ss ={id:'',label:''};
								obj.selectedData.total = '';
								var haveSs = ($(this).attr('subsector') == 'true');
								//solo si esta en nacional maneja subsectores
								if((parseInt(obj.selectedData.geo.id,10) <= 0) && haveSs){
									var _sub = $(this).attr('idref');
									obj.printVarPanelsSub(_sub);
								}
								obj.setChanged();
								obj.refreshVarPanels();
								e.stopPropagation();
							});	
						});
						
						if($.isFunction(func)){
							func();
						}
						
					
				}else{
					obj.printVarEco();
					obj.printPeriodSelection();
				}
				
			});
		}else{
			obj.printVarEco();
			obj.printPeriodSelection();
		}
  },
  printVarPanelsSub:function(sec){
		var obj = this;
		var gs = obj.options.var_gs;
		var sdata = obj.selectedData;
		//funcion para impresion general;
		//si estamos imprimiendo sectores
			var dataSource = $.extend(true,{},obj.options.dataSource.sectors);
			dataSource.url+= '/'+sec;
			
			obj.getData(dataSource,{},function(data){
				var cadena = '';
				if(data.response.success){
					var sectors = data.data.sectors;
					
					obj.serviceData.ssector = $.extend(true,[],data.data.sectors);
					$('#ecoTool_block_ssector').remove();
					cadena = '';
					cadena+='<div id="ecoTool_block_ssector" class="ecoTool-block ecoTool-block-ssector" idref="'+sec+'">';
					cadena+='	<div type="ss" title="Filtre por subsector" class="ecoTool-ssector-title ecoTool-block-title">Subsectores';
					cadena+='	</div>';
					cadena+='	<div class="ecoTool-ssectors-container">';
					for(var x in sectors){
						var sector = sectors[x];
						cadena+='<div '+((sector.id == sdata.ss.id)?'selected="selected"':'')+' idref="'+sector.id+'" type="'+sector.type+'" class="ecoTool-block-item ecoTool-ssector-item">';
						cadena+='	<div>'+sector.name;
						cadena+='	</div>';
						cadena+='</div>';
					}
					cadena+='	</div>';
					cadena+='</div>';
					$('#ecoTool_vars_panel1').append(cadena);
					$('.ecoTool-ssector-title').click(function(){
						obj.adjustBlockSize('ss');
					});
					obj.adjustBlockSize('ss');
					$('.ecoTool-ssector-item').each(function(){
						$(this).click(function(){
							var idref = $(this).attr('idref');
							obj.selectedData.ss = obj.getSector(idref);
							obj.selectedData.total = '';
							obj.setChanged();
							obj.printCurrentValues();
							obj.adjustBlockSize('var');
						});	
					});
					
				}
			});
  },
  printPeriodSelection:function(){
		var obj = this;
		var val = obj.selectedData;
		var years = val.years;
		var cadena = '';
		cadena+='<div id="ecoTool_block_period" class="ecoTool-block ecoTool-block-period">';
		cadena+='	<div type="ss" title="Seleccione el año censal a consultar" class="ecoTool-period-title ecoTool-block-title">Eventos';
		cadena+='	</div>';
		cadena+='	<div class="ecoTool-period-container">';
		for(var x in years){
			cadena+='	<div idref="'+x+'" class="ecoTool-period-item '+((x == val.yearPos)?'selected':'')+'">'+years[x]+'</div>';
		}
		cadena+='	</div>';
		cadena+='</div>';
		$('#ecoTool_vars_panel1').append(cadena);
		$('.ecoTool-period-item').each(function(){
			$(this).click(function(e){
				if(!$(this).hasClass('selected')){
					var idref = parseInt($(this).attr('idref'),10);
					obj.selectedData.yearPos = idref;
					
					$('.ecoTool-period-item.selected').removeClass('selected');
					$(this).addClass('selected');
					obj.setChanged();
					e.stopPropagation();
				}
			})
		});
  },
  printCurrentValues:function(){
		var obj = this;
		var val = obj.currentData;
		var snum = val.strats;
		var rcolors = obj.currentData.colors.colors;
		var cadena = '';
	  
		if(obj.isCollapsed()){
			var total = (val.theme)?val.theme.indicator:0;
			var postfix = obj.getVar(val._var.id).postfix;
			
			/*total = (parseInt(total,10) > 0)?
					 (postfix)?total+postfix:total:'-------';*/
			
			
			
			var gs = obj.getGsVar(val.gs.id);
			var geoNames = [];
			for(var x in val.selection.list){
				geoNames.push(val.selection.list[x].label);	
			}
			
			cadena+= '<div class="ecoTool-currentValues-min">';
			cadena+= '	<div class="ecoTool-cv-var">'+val._var.label+'</div>';
			cadena+= '	<div class="ecoTool-cv-var-val">'+obj.formatMoney(total)+'</div>';
			cadena+= '	<div class="ecoTool-cv-gs">'+gs.label+'</div>';
			cadena+= '	<ul class="ecoTool-cv-list">';
			if(val.s.label != ''){
			cadena+= '		<li class="ecoTool-cv-s">'+val.s.label+'</li>';
			if(val.ss.label != ''){
			cadena+= '			<ul><li class="ecoTool-cv-ss">'+val.ss.label+'</li></ul>';
			}
			cadena+= '		</li>';
			}
			cadena+= '	</ul>';
			//dev
			cadena+= '	<div class="ecoTool-geo"><div class="sprite-ecoTool sprite-ecoTool-mexMap"></div><label>'+geoNames.join()+'</label></div>';
			
			if(val.theme)
				cadena+= '	<div class="ecoTool-min-max"><div class="ecoTool-min-value">'+obj.formatMoney(val.theme.min)+'</div> <div class="ecoTool-max-value">'+obj.formatMoney(val.theme.max)+'</div></div>';
			
			cadena+= '	<div class="ecoTool-colorRamp">';
			for(var x=0;x < snum;x++){
				var width = 100/snum;
				cadena+= '		<div class="ecoTool-colorRamp-item" style="background-color:'+rcolors[x]+';width:'+(width)+'%"></div>';		
			}
			cadena+= '	</div>';
			cadena+= '</div>';
			
		}else{
			val = obj.currentData;
			var total = (val.theme)?val.theme.indicator:0;
			var gs = obj.getGsVar(val.gs.id);
			
			var gs = obj.getGsVar(val.gs.id);
			var geoNames = [];
			for(var x in val.selection.list){
				geoNames.push(val.selection.list[x].label);	
			}
			var postfix = obj.getVar(val._var.id).postfix;
			total = (parseInt(total,10) > 0)?
					 (postfix)?total+postfix:total:'';
			
			cadena+= '<div class="ecoTool-currentValues-ext">';
			cadena+= '	<div class="ecoTool-cv-var-max">'+val._var.label+'</div>';
			cadena+= '	<div class="ecoTool-cv-var-val">'+obj.formatMoney(total)+'</div>';
			cadena+= '	<div id="ecoTool_cv_gs_max" class="ecoTool-cv-gs-max">'+gs.label+'</div>';
			cadena+= '	<div class="ecoTool-cv-s">'+val.s.label+'</div>';
			cadena+= '	<div class="ecoTool-cv-ss">'+val.ss.label+'</div>';
			cadena+= '	<div class="ecoTool-geo-max">'+geoNames.join()+'</div>';
			cadena+= '</div>';
			
			$('.ecoTool-block-item[selected=selected]').each(function(){$(this).removeAttr('selected')})
			if(val.s.id != '')$('.ecoTool-sector-item[idref='+val.s.id+']').attr('selected','selected');
			if(val.ss.id != '')$('.ecoTool-ssector-item[idref='+val.ss.id+']').attr('selected','selected');
			$('.ecoTool-var-item[idref='+val._var.id+']').attr('selected','selected');	
			
			obj.printThemeStats();
			
			
		}
		$('#ecoTool_content_header').html(cadena);
		if(!obj.isCollapsed()){
			$('#ecoTool_content_panels').css({top:($('#ecoTool_content_header').height()+10)+'px'})
		}
  },
  isCollapsed:function(){
		return this.collapsed;
  },
  extendPanel:function(){
		var obj = this;
		var element = obj.element;
		
		var wheight = ($(window).height() < (550+230))?
						$(window).height()-230:550;
						
		obj.configData.maxHeight = wheight;
		
		
		obj.selectedData = $.extend(true,{},obj.currentData);
		if(obj.canResize && obj.isCollapsed()){
			obj.canResize = false;
			element.animate({height:wheight+'px'},function(){
				obj.collapsed = false;
				obj.canResize = true;
				obj.createStructure();
				element.attr('collapsed','false');
				obj.printPanels();
				var tabSel = $('.ecoTool-vars-category[selected=selected]');
				obj.printVarPanels({id:tabSel.attr('idref'),name:tabSel.attr('_name')},function(){
					if(obj.selectedData.ss.id != '')
						obj.printVarPanelsSub(obj.selectedData.s.id);	
				});
				
				obj.options.detectCollision(obj.element);
			});
		}
  },
  rollBack:function(){
	var obj = this;
	if(obj.rollbackColor){
		obj.currentData.colors = $.extend(true,{},obj.rollbackColor);
		obj.changeColorMap();  
		obj.rollbackColor = null;
	}
  },
  collapsePanel:function(opc){
		var obj = this;
		var element = obj.element;
		
		/*if(opc === undefined){
			obj.currentData = $.extend(true,{},obj.selectedData);
			if(obj.hasChanged){
				obj.mapTheme();//rem
			}
		}else{
			obj.rollBack();
		}*/
		if(obj.canResize && !obj.isCollapsed()){
			obj.canResize = false;
			element.animate({height:obj.configData.minHeight+'px'},function(){
				obj.collapsed = true;
				obj.canResize = true;
				obj.createStructure();
				element.attr('collapsed','true');
				var height = ($('#ecoTool_content_header').height()+10 > 120)?$('#ecoTool_content_header').height()+10:120;
				element.animate({height:height+'px'});
				
				obj.printCurrentValues();
			});
		}
  },
  exportStrats:function(type){
	  var obj = this;
	  var data = obj.currentData;
	  var theme = data.theme;
	  if(theme){
		  var _var = data._var.label;  
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
		  var ds = $.extend({},true,obj.options.dataSource.exportFicha);
		  var params = {title:'Detalle de Estratos',columns:columns,values:values};
		  obj.getData(ds,params,function(data){
			  if(data && data.response.success){
			  	var url = ds.urlGet+'/'+type+'/'+data.data.id;
			  	window.location.assign(url);
			  }
		  });
		  obj.getData();
	  }
  },
 //Conversion
 stringAsType:function(text){
					text = text.toLowerCase();
					text = text.replace(/[\u00E1]/gi,'a');
					text = text.replace(/[\u00E9]/gi,'e');
					text = text.replace(/[\u00ED]/gi,'i');
					text = text.replace(/[\u00F3]/gi,'o');
					text = text.replace(/[\u00FA]/gi,'u');
					text = text.replace(/[\u00F1]/gi,'n');

					text = text.replace(/&aacute;/g, 'a');
					text = text.replace(/&eacute;/g, 'e');
					text = text.replace(/&iacute;/g, 'i');
					text = text.replace(/&oacute;/g, 'o');
					text = text.replace(/&uacute;/g, 'u');
					text = text.replace(/&ntilde;/g, 'n');

					text = text.replace(/,/g, '');

					text = text.replace(/\s/g, '');
					return text;
  },
  spinner:function(option){
		var obj = this;
		if(option == 'show'){
			if(!$('#ecoTool_spinner_panel').attr('id')){
				var w = obj.element.width();
				var h = obj.element.height();
				var cadena = '<div id="ecoTool_spinner_panel" class="ecoTool-spinner-panel" count="1" style="width:'+w+'px;height:'+h+'px">';
					cadena+= '	<div class="ui-widget-overlay ecoTool-block-overlay"></div>';
					cadena+= '	<div class="ecoTool-spinner-image-container"><span class="ecoTool-spinner-image"></div>';
					cadena+= '<div>';

				obj.element.append(cadena);	
			}else{
				var count = parseInt($('#ecoTool_spinner_panel').attr('count'),10);
				$('#ecoTool_spinner_panel').attr('count',count+1);

			}
		}else{
			if($('#ecoTool_spinner_panel').attr('id')){
				var count = parseInt($('#ecoTool_spinner_panel').attr('count'),10);
				if(count > 1){
					$('#ecoTool_spinner_panel').attr('count',count-1);	
				}else{
					$('#ecoTool_spinner_panel').remove();
				}
			}
		}
  },
  getData:function(source,params,callback,error,before,complete){
		var obj = this;
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
	},
  // the constructor
  _create: function() {
	 var obj = this;
	 obj.element
	  // add a class for theming
	  .addClass( "custom-ecoTool" ).attr('collapsed','true');
	  // prevent double click to select text
	 obj.currentData = obj.options.defaultData;
	 obj.selectedData.original_strats = obj.currentData.strats;
	 
	 obj.configData.colorRamps = obj.options.colorRamps;
	 obj.currentData.colors = obj.configData.colorRamps[0];
	 obj.configData.edos = obj.options.edos; 
	 
	 obj.id = this.element.attr('id');
	 obj.createStructure();
	 obj.mapTheme();
	 obj.getDataInfoVars();
	 var height = ($('#ecoTool_content_header').height()+10 > 120)?$('#ecoTool_content_header').height()+10:120;
	 obj.element.animate({height:height+'px'});
	 obj.options.onStart();
	 obj._refresh();
	 
	 
	 obj.extentToCvegeo(obj.currentData.geo.id);
  },

  // called when created, and later when changing options
  _refresh: function() {
	// trigger a callback/event
  },
  setChanged:function(){
	var obj = this;
	obj.selectedData.theme.indicator = ''; 
	obj.currentData = $.extend({},true,obj.selectedData);
	obj.hasChanged = true;
	//$('#ecoTool_content').attr('changed','true');
	
	clearTimeout(obj.applyChangesTimer);
	obj.applyChangesTimer = setTimeout(function(){
		obj.mapTheme();
		//obj.printPanels();
	},600);
  },
  // events bound via _on are removed automatically
  // revert other modifications here
  _destroy: function() {
	this.options.onClose();
	this.element
	  .removeClass( "custom-ecoTool" )
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