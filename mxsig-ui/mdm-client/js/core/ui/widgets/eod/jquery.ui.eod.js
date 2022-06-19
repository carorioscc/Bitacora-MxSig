$.widget("custom.eod", {
	// default options
	options: {
		config: null,
		mapLayer:null,
		onActive: function () {

		},
		onClose: function () {

		}
	},
	mainGraphSortBy: 'value',
	mainGraphSortReverse: { //determina que ordenado esta invertido
		name: false,
		value: true
	},
	storedData:[],
	geoLists: {},
	timeList: [],
	currentData: null,
	typeOD: 'oneall', //sentido de la consulta, un origen a muchos destinos o viceversa  ONEALL, ALLONE por defecto basado en la configuracion de inicio de una entidad a muchos
	hasChanged: false,
	showStratsMessage: false,
	firstBoot: true,
	timePlaying: false,
	playAll: true, //considera acumulado en reproduccion
	timerPlayGraph: 0,
	graphTimePlay: 4000,
	forceBanAllDay:true,
	triggerChanges: true,
	silentServices:false, //evita que se presente el spinner de carga, se reinicia en cada peticion
	storeDataService:true, //almacena el resultado de una consulta
	// the constructor
	_create: function () {
		var obj = this;
		obj.createTimeList();
		obj.id = obj.element.attr('id');
		obj.clearThemeLayer();

		//changeDefault geo  
		var pos = MDM6('getMyLocation');

		obj.options.config.config.startingData.geoSelected = '00'; //[pos.cityCode];
		obj.element.attr('cantheme', false); //deshabilita paneles si viene en 00
		obj.element.attr('expanded', false); //deshabilita paneles si viene en 00

		obj.currentData = $.extend({}, true, obj.options.config.config.startingData);
		obj.settings = $.extend({}, true, obj.options.config.config.settings);

		this.element
			// add a class for theming
			.addClass("custom-eod toolCustomIdentify eod-animated").attr('collapsed', 'true').attr('seltab', 'geo').attr('changed', 'false').attr('geotype', obj.currentData.geoType)
			.attr('infopanel', 'data')
			// prevent double click to select text
			.disableSelection();
		this.id = this.element.attr('id');

		obj.element.addClass('no-print');

		obj.options.onStart();
		obj.createUI();

		//inicia tema al iniciar
		//obj.prepareTheme();

		this.options.onActive();

		this._refresh();

		obj.options.extent("POLYGON((-11072291.4017239 2150265.71209335,-11072291.4017239 2270253.40916838,-10988811.6458699 2270253.40916838,-10988811.6458699 2150265.71209335,-11072291.4017239 2150265.71209335))");
		var url = obj.options.config.config.settings.bootDialog;

		//reviza integridad de datos por defecto y de ser satisfactorio genera graficado
		obj.checkSelectionIntegrity();

		var url = obj.options.config.config.settings.bootDialog;
		
		obj.openDialog(url, 'Encuesta Origen - Destino en Hogares de la Zona Metropolitana del Valle de México 2017');
		
		setTimeout(function(){
			obj.openConfig();
		},1000);
		
	},

	// called when created, and later when changing option
	_refresh: function () {
		// trigger a callback/event
		var obj = this;
	},

	// events bound via _on are removed automatically
	// revert other modifications here
	_destroy: function () {
		this.options.onClose();
		this.onClose();
		this.element
			.removeClass("custom-eod")
			.enableSelection();
	},

	// _setOptions is called with a hash of all options that are changing
	// always refresh when changing options
	_setOptions: function () {
		// _super and _superApply handle keeping the right this-context
		this._superApply(arguments);
		this._refresh();
	},

	// _setOption is called for each individual option that is changing
	_setOption: function (key, value) {
		// prevent invalid color values
		this._super(key, value);
	},
	getTimeInfo: function (time) {
		var obj = this;
		if (time == '24:00')
			time = '23:59';


		var list = obj.timeList;
		var r = null;
		for (var x in list) {
			var item = list[x];
			if (item.militia == time) {
				r = item;
				break;
			}
		}
		return r;
	},
	getTimeDifference: function (init, end) {
		var obj = this;
		var dif = (new Date("2018-1-1 " + init) - new Date("2018-1-1 " + end)) / 1000 / 60
		return dif;
	},
	getTimeAbove: function (time, stop) { //24hrs time
		var obj = this;

		var cData = $.extend({}, obj.currentData);
		var min = (cData.schedule.init) ? cData.schedule.init : '00:00';
		var max = (cData.schedule.end) ? cData.schedule.end : '23:59';
		max = parseInt(max.replace(':', ''));

		var _time = time;
		time = parseInt(time.replace(':', ''));
		var list = obj.timeList.slice();
		var r = null;
		var last = list[0];
		var rlist = [];
		for (var x in list) {
			var item = list[x];
			var militia = parseInt(item.militia.replace(':', ''));
			var disabled = true;
			if (militia < time) {
				item.below = true;
				disabled = false;
			}
			if (militia < max) {
				disabled = false;
			}
			item.disabled = disabled;
			//item.diff = obj.getTimeDifference(_time,item.militia);
			var diff = obj.getTimeDifference(_time, item.militia);
			if (diff == 0) {
				item.diff = '';
			} else {
				if (Math.abs(diff) == 1439)
					diff = (diff < 0) ? (-1440) : (1440);
				
				diff = diff / 60;
				if((diff.toFixed(2)).split('.')[1] == '98')
					diff = diff+0.02;
				

				item.diff = ((diff < 0) ? '+' : '-') + '(' + (Math.abs(diff)).toFixed(2) + ') hrs. ';
			}
			rlist.push(item)

		}
		return rlist;
	},
	getTimeBelow: function (time, stop) { //24hrs time
		var obj = this;

		var cData = $.extend({}, obj.currentData);
		var min = (cData.schedule.init) ? cData.schedule.init : '00:00';
		var max = (cData.schedule.end) ? cData.schedule.end : '23:59';
		min = parseInt(min.replace(':', ''));

		var _time = time;
		time = parseInt(time.replace(':', ''));

		var list = obj.timeList.slice();
		var r = null;
		var last = list[0];
		var rlist = [];
		for (var x in list) {
			var item = list[x];
			var militia = parseInt(item.militia.replace(':', ''));
			var disabled = true;
			if (militia > time) {
				item.above = true;
			}
			if (militia > min) {
				disabled = false;
			}
			item.disabled = disabled;
			//item.diff = obj.getTimeDifference(_time,item.militia);
			var diff = obj.getTimeDifference(_time, item.militia);
			if (diff == 0) {
				item.diff = '';
			} else {
				if (Math.abs(diff) == 1439)
					diff = (diff < 0) ? (-1440) : (1440);
				
				diff = diff / 60;
				
					
				if(parseInt((diff.toFixed(2)).split('.')[1])%5 != 0)
					if(diff > 0 ){
						diff = diff+0.02;
					}else{
						diff = diff-0.02;
					}

				item.diff = '(' + ((diff < 0) ? '+' : '-') + (Math.abs(diff)).toFixed(2) + ')';
			}
			rlist.push(item)

		}
		return rlist;
	},
	getNearTime: function (time) { //24hrs time
		var obj = this;
		time = parseInt(time.replace(':', ''));
		var list = obj.timeList;
		var r = null;
		var last = list[0];
		for (var x in list) {
			var item = list[x];
			var militia = parseInt(item.militia.replace(':', ''));
			if (militia >= time) {
				if (militia == time) {
					r = item;
				} else {
					r = last;
				}
				break;
			}
			last = item;
		}
		return r;
	},
	createTimeList: function () {
		var obj = this;
		obj.timeList = [];
		var count = 0;
		for (var m = 0; m < 2; m++) {
			var mer = (m == 0) ? 'am' : 'pm';
			for (var x = 0; x < 12; x++) {
				for (var y = 0; y < 4; y++) {
					var h = (mer == 'pm' && x == 0) ? 12 : x;
					var min = (y < 1) ? '00' : (y * 15);
					var th = (h < 10) ? '0' + h : h;
					var mh = (12 * m) + x;
					mh = (mh < 10) ? '0' + mh : mh
					var value = th + ':' + min + ' ' + mer;

					//---------------------------- 
					var _mh = (12 * m) + x;
					if (min == 00)
						_mh--;
					_mh = (mh < 10) ? '0' + _mh : '' + _mh
					var _min = (y < 1) ? '59' : (y * 15) - 1;
					_mh = (_mh.length == 1) ? '0' + _mh : _mh;

					var _value = _mh + ':' + _min;
					if (h == '00' && min == '00') {
						_value = '00:00';
					}

					//---------------------------- 
					obj.timeList.push({
						id: value,
						value: value,
						militia: mh + ':' + min,
						militia_end: _value,
						position: count
					});
					count++;
				}
			}
		}
		obj.timeList.push({
			id: '11:59 pm',
			value: '11:59' + ' pm',
			militia: '23:59',
			militia_end: '23:59',
			position: obj.timeList.length
		});
	},
	createUI: function () {
		var obj = this;
		var cd = obj.currentData;

		//obj.printGeoList();
		var cadena = '<div id="eod_header" class="eod-header eod-resize eod-transition">';
		cadena += '<div id="eod_header_click_collapsed"></div>';
		cadena += '<div id="eod_header_logoLeft" idref="" class="eod-logo-left" title="Encuesta Origen - Destino en Hogares de la Zona Metropolitana del Valle de México 2017"></div>';
		cadena += '<div id="eod_header_title" class="eod-truncate-text"><div id="eod_var_title"></div><div id="eod_subvar_title"></div></div>';
		cadena += '<div class="eod-header-btns">';
		cadena += '	<div id="eod_header_btnRight" idref="" class="eod-header-btn">';
		cadena += '		<div id="eod_header_btnRight_modify" idref="" class="eod-header-btn-inner sprite-eod sprite-eod-modify"></div>';
		cadena += '		<div id="eod_header_btnRight_ok_nm" idref="" class="eod-header-btn-inner sprite-eod sprite-eod-down"></div>';
		cadena += '		<div id="eod_header_btnRight_ok_n" idref="" class="eod-header-btn-inner sprite-eod sprite-eod-down"></div>';
		cadena += '	</div>';
		cadena += '	<div id="' + obj.id + '_header_btnRight_close" idref="" class="eod-header-btnRight-close eod-header-btn-inner sprite-eod sprite-eod-info"></div>';
		cadena += '</div>';
		//cadena+=	'<div class="eod-header-colors" id="eod_color_container">';
		/*cadena+=	'<div class="eod-header-colors-years-label"><div id="eod-year-min" class="eod-header-year-label-left"></div><div id="eod-year-max" class="eod-header-year-label-right"></div></div>';

			var colors = obj.currentData.colors.colors;
	   		for(var x in colors){
				var color = colors[x];
				cadena+=	'<div class="eod-header-colors-item" style="background-color:'+color+';width:'+Math.floor(100/colors.length)+'%"></div>';
			}
	   
			cadena+=	'</div>';*/

		//cadena+= '</div>';
		cadena += '<div id="eod_panels_container" class="eod-panels-container eod-animated" panel="vars">';
		cadena += '	<div id="eod_container" class="eod-container eod-tab-container" parents="false" typeselection="' + obj.currentData.typeVarSelection + '">';
		cadena += '		<div id="eod_content" class="eod-resize eod-animated eod-content"><div class="eod-legend"></div></div>';
		cadena += '		<div id="eod_bk_btn" class="eod-resize eod-animated eod-bk-btn"><div class="eod-bk-btn-icon sprite-eod sprite-eod-bback"></div></div>';
		cadena += '	</div>';
		cadena += '	<div id="eod_geo_container" class="eod-geo-container eod-tab-container">';
		cadena += '		<div id="eod_geo_bk_btn" class="eod-resize eod-animated eod-geo-bk-btn"><div class="eod-geo-bk-btn-icon sprite-eod sprite-eod-bback"></div></div>';
		cadena += '		<div id="eod_geo_content" class="eod-resize eod-animated eod-geo-content"></div>';
		cadena += '		<div id="eod_geo_type" class="eod-resize eod-animated eod-geo-type">';
		cadena += '		</div>';
		cadena += '	</div>';
		cadena += '	<div id="eod_conf_container" class="eod-conf-container eod-tab-container">';
		cadena += '		<div id="eod_conf_content" class="eod-resize eod-animated eod-conf-content"></div>';
		cadena += '	</div>';
		cadena += '	<div id="eod_info_container" class="eod-info-container eod-tab-container">';
		cadena += '		<div id="eod_info_content" class="eod-resize eod-animated eod-info-content"></div>';
		cadena += '		<div id="eod_info_graph" class="eod-resize eod-animated eod-info-graph"></div>';
		cadena += '	</div>';
		cadena += '</div>';

		cadena += '<div id="eod_toolbar_container" class="eod-toolbar-container eod-animated">';
		cadena += '	<div id="eod_tab_geo" idref="geo" class="eod-resize eod-tab"><div class="sprite-eod-geo"></div><div>Configurar</div></div>';
		cadena += '	<div id="eod_tab_graph" idref="graph" class="eod-resize eod-tab"><div class="sprite-eod-graph"></div><div>Gráfica</div></div>';
		cadena+= '	<div id="eod_tab_vars" idref="var" class="eod-resize eod-tab"><div class="sprite-eod-legend"></div><div>Leyenda</div></div>';
		cadena+= '	<div id="eod_tab_info" idref="info" class="eod-resize eod-tab"><div class="sprite-eod-big-info"></div><div>Más información</div></div>';
		
		/*
		cadena += '	<div id="eod_tab_bottom_container" idref="info" class="eod-tab-bottom-container eod-animated">';
		cadena += '		<a href="' + obj.settings.mainDoc + '" target="_blank">';
		cadena += '			<div id="eod_tab_pdfdoc" idref="pdfdoc" class="">';
		cadena += '				<div class="eod-resize sprite-eod-doc-pdf" title="Nota metodológica"></div>';
		cadena += '				<div class="eod-doc-label" title="Nota metodológica" >Nota</div>';
		cadena += '			</div>';
		cadena += '		</a>';
		cadena += '	</div>';
		*/
		
		
		//cadena+= '	<div class="eod-disable-toolbars"></div>';
		cadena += '</div>';


		obj.element.html(cadena);

		//obj.updateRampStrat(); 

		$('#eod_header_click_collapsed').click(function () {
			$('#eod_header_btnRight').click();
		});

		$('#' + obj.id + '_header_btnRight_close').click(function (e) {
			var url = obj.options.config.config.settings.bootDialog;
			obj.openDialog(url, 'Encuesta Origen - Destino en Hogares de la Zona Metropolitana del Valle de México 2017');
			e.stopPropagation();
		});

		$('.eod-tab').each(function () {
			$(this).click(function (e) {
				var idref = $(this).attr('idref');
				obj.element.attr('seltab', idref);

				$('#eod_weekgraph_main_container').remove();
				obj.printUIConfig();
				clearTimeout(obj.timerPlayGraph);
				obj.timePlaying = false;
				obj.element.attr('expanded', 'false');

				e.stopPropagation();
			})
		});


		$('#eod_header').click(function (e) {
			if ($('#eod_panels_container').height() == 0) {
				//obj.openConfig();
				e.stopPropagation();
			}
		});


		$('#eod_header_btnRight').click(function () {
			if ($('#eod_panels_container').height() == 0) {
				obj.openConfig();
			} else {
				obj.closeConfig();
			}
		});
		$('#eod_bk_btn').click(function () {
			if (obj.currentData.tree.length > 1) {
				obj.currentData.tree.pop();
				if (obj.currentData.tree.length <= 1) {
					$('#eod_container').attr('parent', 'false');
				}
				obj.loadTree(obj.currentData.tree[obj.currentData.tree.length - 1].id, true);
			}
		});

		obj.element.fadeIn();
		obj.printUIConfig();

	},
	printUIConfig: function () {
		var obj = this;
		var cData = obj.currentData;
		var week = cData.weekDay;
		var time = cData.schedule;
		var source = cData.from;
		var target = cData.to;
		var isInternal = cData.internals;

		var cadena = '';
		cadena += '<div class="eod-config-title">Seleccione los días de viaje</div>';
		cadena += '<div class="eod-config-week-container">';
		//cadena += '	<div class="eod-config-week-item eod-config-selectable" idref="all" title="martes o miércoles o jueves y sábado">TODOS LOS DÍAS</div>';
		cadena += '	<div class="eod-config-week-item eod-config-selectable" idref="inner" title="Viajes realizados en un día entre semana (martes, miércoles o jueves)">DÍA ENTRE SEMANA</div>';
		cadena += '	<div class="eod-config-week-item eod-config-selectable" idref="sat" title="Viajes realizados en día sábado">DÍA SÁBADO</div>';
		cadena += '</div>';

		cadena += '<div class="eod-config-title">Viajes</div>';

		//contenedor de bloque de viaje e inclusion de boton a la derecha para hacer el intercambio de rutas

		cadena += '<div class="eod-config-paths-container">';

		cadena += '<div class="eod-config-source-container">';
		cadena += '	<div class="eod-config-subtitle">Origen de viaje</div>';
		cadena += '	<div class="eod-config-source-item eod-config-source-option eod-config-selectable" idref="ent" title="Entidad Federativa de la ZMVM">ENTIDAD</div>';
		cadena += '	<div class="eod-config-source-item eod-config-source-option eod-config-selectable" idref="mun" title="Municipios o Delegaciones de la ZMVM">MUNICIPIO</div>';
		cadena += '	<div class="eod-config-source-item eod-config-source-option eod-config-selectable" idref="dis" title="Distritos de la ZMVM">DISTRITO</div>';
		cadena += '	<div class="eod-config-source-item" idref="sel" id="eod_config_source_geo_select"></div>';
		cadena += '</div>';

		cadena += '<div id="eod_config_target_container" class="eod-config-target-container">';
		cadena += '	<div class="eod-config-subtitle">Destino de viaje</div>';
		cadena += '	<div class="eod-config-target-item eod-config-target-option eod-config-selectable" idref="ent" title="Entidad Federativa de la ZMVM">ENTIDAD</div>';
		cadena += '	<div class="eod-config-target-item eod-config-target-option eod-config-selectable" idref="mun" title="Municipios o Delegaciones de la ZMVM">MUNICIPIO</div>';
		cadena += '	<div class="eod-config-target-item eod-config-target-option eod-config-selectable" idref="dis" title="Distritos de la ZMVM">DISTRITO</div>';
		cadena += '	<div class="eod-config-target-item" idref="sel" id="eod_config_target_geo_select"></div>';
		cadena += '</div>';

		cadena += '</div>';

		cadena += '<div id="eod_config_paths_righttool_container" class="eod-config-paths-righttool-container" title="Intercambiar origen y destino">';
		cadena += '	<div id="eod_config_paths_righttool" class="eod-config-paths-righttool sprite-eod-swap" title="Intercambiar origen y destino"></div>';
		cadena += '</div>';

		cadena += '<div class="eod-config-title" title="Viajes que tienen como origen y destino una misma área geográfica (Entidad, Municipio o Distrito).">Viajes internos</div>';
		cadena += '<div class="eod-config-traveltype-container">';
		cadena += '	<div class="eod-config-traveltype-item eod-config-traveltype-option eod-config-selectable" idref="false">NO INCLUIR</div>';
		cadena += '	<div class="eod-config-traveltype-item eod-config-traveltype-option eod-config-selectable" idref="true">INCLUIR</div>';
		cadena += '</div>';

		cadena += '<div class="eod-config-title">Horario</div>';
		cadena += '<div class="eod-config-schedule-container">';
		cadena += '	<div id="eod_config_schedule_btn" class="eod-config-schedule-item eod-config-selectable" idref="all" title="Incluye los viajes que se realizan en horario de inicio o término -no especificado-.">TODO EL DÍA</div>';
		cadena += '	<div class="eod-config-schedule-item" idref="init" id="eod_config_init_time_select_container">' + cData.schedule.init + '</div>';
		cadena += '	<div class="eod-config-schedule-item" idref="end" id="eod_config_end_time_select_container">' + cData.schedule.end + '</div>';
		cadena += '</div>';

		$('#eod_geo_content').html(cadena);
		
		obj.updateUIPpal();

		//fija valores activos

		$('.eod-config-week-item[idref=' + week + ']').addClass('active');

		if (time.type == 'all')
			$('.eod-config-schedule-item[idref=' + time.type + ']').addClass('active');

		$('.eod-config-source-option[idref=' + source.type + ']').addClass('active');
		$('.eod-config-target-option[idref=' + target.type + ']').addClass('active');

		if (isInternal) {
			$('.eod-config-traveltype-item[idref=true]').addClass('active');
		} else {
			$('.eod-config-traveltype-item[idref=false]').addClass('active');
		}

		//interccambio de rutas
		$('#eod_config_paths_righttool_container').click(function () {
			obj.swapPaths();
			obj.printUIConfig();
		})

		//eventos
		$('#eod_config_init_time_select_container').click(function () {
			var container = $(this);
			obj.showGraphWeek();
			/*obj.createWatch(time.init,'init',function(time){
			 $('#eod_config_schedule_btn').removeClass('active');
			 
			 cData.schedule.type = 'range';
		 	 cData.schedule.init = time.hours+':'+time.mins+' '+time.merid;
			 
			 if(cData.schedule.end == null){
				 cData.schedule.end = '11:59 pm';
				  $('#eod_config_end_time_select_container').html(cData.schedule.end);
			 }
			 
			 container.html(cData.schedule.init);
			 
			 obj.checkSelectionIntegrity('initTime');
		 }); */
		});
		$('#eod_config_end_time_select_container').click(function () {
			var container = $(this);
			obj.showGraphWeek();
			/*
		 obj.createWatch(time.end,'end',function(time){
			 $('#eod_config_schedule_btn').removeClass('active');
			 cData.schedule.type = 'range';
		 	 cData.schedule.end = time.hours+':'+time.mins+' '+time.merid;
			 container.html(cData.schedule.end);
			 
			 obj.checkSelectionIntegrity('endTime');
		 }); */
		});
		$('.eod-config-week-item').each(function () {
			$(this).click(function () {
				var idref = $(this).attr('idref');
				cData.weekDay = idref;
				$('.eod-config-week-item.active').removeClass('active');
				$(this).addClass('active');

				obj.checkSelectionIntegrity();
			});
		});
		//viajes internos
		//horario----------------------------
		$('.eod-config-traveltype-option').each(function () {
			$(this).click(function () {
				var active = ($(this).attr('idref') == 'true');
				cData.internals = active;

				$('.eod-config-traveltype-option.active').removeClass('active');
				$(this).addClass('active');
				obj.checkSelectionIntegrity();
			});
		});

		$('#eod_config_schedule_btn').click(function () {
			var idref = $(this).attr('idref');
			obj.forceBanAllDay = true;
			cData.schedule.type = idref;
			cData.schedule.init = '00:00';
			cData.schedule.end = '23:59';
			$(this).addClass('active');

			obj.updateUIPpal();

			obj.checkSelectionIntegrity();
		});
		//geo-----------------------------------------

		$('.eod-config-source-option').each(function () {
			$(this).click(function () {
				var idref = $(this).attr('idref');
				
				if(cData.from.cvegeo != 'all'){
					cData.from.cvegeo = null;
					obj.typeOD = 'oneall';
				}else{
					obj.typeOD = 'allone';
				}
				
				cData.from.type = idref;
				
				
				//cData.to.cvegeo = 'all';

				$('.eod-config-source-option.active').removeClass('active');
				$(this).addClass('active');
				obj.printGeoList('source', idref, function () {
					
					
					//obj.printGeoList('target', source.type);
					
					obj.validateSourceTarget('source'); //notifica quien cambio
					obj.checkSelectionIntegrity();
				});
			});
		});

		$('.eod-config-target-option').each(function () {
			$(this).click(function () {
				var idref = $(this).attr('idref');
				
				if(cData.to.cvegeo != 'all'){
					cData.to.cvegeo = null;
					obj.typeOD = 'allone';
				}else{
					obj.typeOD = 'oneall';
				}

				cData.to.type = idref;
				
				//cData.from.cvegeo = 'all';

				$('#eod_config_target_container').attr('option', idref);
				$('.eod-config-target-option.active').removeClass('active');
				$(this).addClass('active');
				obj.printGeoList('target', idref, function () {
					obj.typeOD = 'allone';
					
					//obj.printGeoList('source', source.type);
					
					obj.validateSourceTarget('target'); //notifica quien cambio
					obj.checkSelectionIntegrity();
				});
			});
		});
		//imprimir actuales
		obj.printGeoList('source', source.type, function () {
			obj.printGeoList('target', target.type);
		});
		obj.printInfoTab();

	},
	printInfoTab:function(){
		var obj = this;
		var cadena = '<table class="eod-info-table" width="100%" border="0">';
			cadena+='	<tbody>';
			cadena+='		<tr>';
			cadena+='			<th colspan="3"></th>';
			cadena+='		</tr>';
			cadena+='		<tr>';
			cadena+='			<td align="center" bgcolor="#92a5cf">Nota técnica</td>';
			cadena+='			<td>&nbsp;</td>';
			cadena+='			<td align="center" bgcolor="#92a5cf">Más información</td>';
			cadena+='		</tr>';
			cadena+='		<tr>';
			cadena+='			<td align="center">';
			cadena+='				<a href="'+obj.settings.mainDoc+'" target="_blank">';
			cadena+='					<div class="eod-resize sprite-eod-doc-pdf-mini"></div>';
			cadena+='				</a>';
			cadena+='			</td>';
			cadena+='			<td>&nbsp;</td>';
			cadena+='			<td align="center">';
			cadena+='				<a href="'+obj.settings.microdataDoc+'" target="_blank">';
			cadena+='					<div id="eod_btn_notice" class="agropecuario-resize sprite-eod-info"></div>';
			cadena+='				</a>';
			cadena+='			</td>';
			cadena+='		</tr>';
			cadena+='	</tbody>';
			cadena+='</table>';
		
			$('#eod_info_content').html(cadena);
		
	},
	swapPaths: function () {
		var obj = this;
		var cData = obj.currentData;
		var source = cData.from;
		var target = cData.to;

		cData.from = target;
		cData.to = source;

		obj.checkSelectionIntegrity();

	},
	validateSourceTarget: function (changed,priority) {  //quien cambio y si tiene prioridad
		var obj = this;
		var cData = obj.currentData;
		var source = cData.from;
		var target = cData.to;

		var listSource = obj.geoLists[source.type];
		var listTarget = obj.geoLists[target.type];

		var objsource = $('#eod_source_geo_list');
		var objtarget = $('#eod_target_geo_list');

		var type = obj.typeOD;
		
		var isAllLocal = ({source:source,target:target}[changed].cvegeo == 'all');  //determina si quien se cambio apunta a todos
		var isAllOther = ({source:target,target:source}[changed].cvegeo == 'all'); //determina si quien el otro punto es todos
		var isBothAll = (source.cvegeo == 'all' && target.cvegeo == 'all');
		var noneAll = (source.cvegeo != 'all' && target.cvegeo != 'all');
		var areSame = (source.cvegeo == target.cvegeo);
		

		var getAvalibleCve = function () {
			var list = (type == 'oneall') ? listSource : listTarget;
			var unique = (type == 'oneall') ? source.cvegeo : target.cvegeo;
			var r = null;
			for (var x in list) {
				if (list[x].cvegeo != 'all' && list[x].cvegeo != unique) {
					r = list[x].cvegeo
					break;
				}
			}
			return r;
		}
		
		//el supuesto de que ambos sean destino
		if (isBothAll && type == 'oneall') {
			var cvesel = getAvalibleCve();
			cData.from.cvegeo = cvesel;
			$('#eod_source_geo_list option:selected').removeProp('selected');
			$('#eod_source_geo_list option[value=' + cvesel + ']').prop('selected', 'selected');

		};
		//el supuesto de que ambos sean destino
		if (isBothAll && type == 'allone') {
			var cvesel = getAvalibleCve();
			cData.to.cvegeo = cvesel;
			$('#eod_target_geo_list option:selected').removeProp('selected');
			$('#eod_target_geo_list option[value=' + cvesel + ']').prop('selected', 'selected');
		};

		if (noneAll && areSame) {
			cData.internals = true;
			$('.eod-config-traveltype-option.active').removeClass('active');
			$('.eod-config-traveltype-option[idref=true]').addClass('active');
		};



	},
	printGeoList: function (type, typegeo, func) {
		var obj = this;
		var cData = obj.currentData;

		var searched = false;
		var list = obj.geoLists[typegeo];
		var print = function (type, typegeo) {
			list = obj.geoLists[typegeo];
			var cadena = '<select id="eod_' + type + '_geo_list" class="eod-config-edo-list">';
			//if(type == 'target')
			cadena += '<option value="all">Todos</option>';

			for (var x in list) {
				var item = list[x];
				cadena += '<option value="' + item.cvegeo + '">' + item.nomgeo + '</option>';
			}
			cadena += '</select>';

			if (type == 'source') {
				$('#eod_config_source_geo_select').html(cadena);
				if (cData.from.cvegeo == null) {
					var first = list[0];
					if (first)
						cData.from.cvegeo = first.cvegeo;
				}
				$('#eod_config_source_geo_select option[value=' + cData.from.cvegeo + ']').prop('selected', 'selected');

				//combo selections
				$('#eod_source_geo_list').change(function () {
					var selected = $('#eod_source_geo_list option:selected').val();
					if(selected == 'all' && cData.to.cvegeo == 'all'){
						cData.to.cvegeo == null		
					}
					cData.from.cvegeo = selected;

					if (cData.from.cvegeo == 'all') {
						obj.typeOD = 'allone';
					} else {
						if (cData.from.cvegeo != 'all' && cData.to.cvegeo != 'all') obj.typeOD = 'oneone';
					}

					obj.validateSourceTarget('source',true);

					obj.checkSelectionIntegrity();
				});

			} else {
				if (cData.to.cvegeo == null) {
					var first = list[0];
					if (first)
						cData.to.cvegeo = first.cvegeo;
				}
				
				
				$('#eod_config_target_geo_select').html(cadena);
				$('#eod_config_target_geo_select option[value=' + cData.to.cvegeo + ']').prop('selected', 'selected');

				$('#eod_target_geo_list').change(function () {
					var selected = $('#eod_target_geo_list option:selected').val();
					
					if(selected == 'all' && cData.from.cvegeo == 'all'){
						cData.from.cvegeo == null		
					}
					cData.to.cvegeo = selected;

					if (cData.to.cvegeo == 'all') {
						obj.typeOD = 'oneall';
					} else {
						if (cData.from.cvegeo != 'all' && cData.to.cvegeo != 'all') obj.typeOD = 'oneone';
					}

					obj.validateSourceTarget('target',true);

					obj.checkSelectionIntegrity();
				});

			}
			if ($.isFunction(func))
				func();
		}

		if (list) {
			print(type, typegeo);
		} else {
			if (!searched) {
				var ds = obj.options.config.dataSources;
				var dataSource = $.extend(true, {}, ds.getGEO);
				obj.getData(dataSource, {
					level: typegeo
				}, function (data) {
					if (data.response.success) {
						obj.geoLists[typegeo] = data.data.geo;
						print(type, typegeo);
					}
				});
			} else {

			}
		}

	},
	checkSelectionIntegrity: function (opc) { //revisa la integridad de los datos antes de enviar a tematizar
		var obj = this;
		var cData = obj.currentData;

		var week = cData.weekDay;
		var time = cData.schedule;
		var source = cData.from;
		var target = cData.to;

		var valTimeIni = obj.getTimeInfo(time.init);
		var valTimeEnd = obj.getTimeInfo(time.end);

		//de existir horas y estas traslaparse 
		if (valTimeIni || valTimeEnd) {
			if (!valTimeIni) {
				valTimeIni = obj.timeList[0];
				time.init = valTimeIni.militia;
				$('#eod_config_init_time_select_container').html(time.init);
			}


			if (valTimeIni.position >= valTimeEnd.position) {
				if (opc == 'initTime') {

				}
				if (valTimeIni.position < obj.timeList.length - 1) {
					valTimeEnd = obj.timeList[valTimeIni.position + 1];
				} else {
					valTimeIni = obj.timeList[valTimeEnd.position - 1];
				}
				time.init = valTimeIni.militia;
				time.end = valTimeEnd.militia_end;
				
				obj.updateUIPpal();

			}
		}

		valTimeIni = (time.init) ? parseInt(time.init.replace(':', '')) : 0;
		valTimeEnd = (time.end) ? parseInt(time.end.replace(':', '')) : 0;

		var banWeek, banTime, banSource, banTarget = false; //reinicio de variables bandera;

		banWeek = (['all', 'inner', 'sat'].indexOf(week) >= 0); //si el valor del dia de la seman se encuentra en el vector
		// banTime = (time.type == 'all' || (time.type == 'range' && time.init != null  && time.end != null && valTimeIni < valTimeEnd)); //si tiene seleccionaod una hora real
		banSource = (['ent', 'mun', 'dis'].indexOf(source.type) >= 0 && source.cvegeo != ''); //revisa valor de origen 
		banTarget = (['ent', 'mun', 'dis'].indexOf(target.type) >= 0 && target.cvegeo != ''); //revisa valor de origen 

		//ajuste de internos
		/* var isOnetoOne = (source.cvegeo != 'all' && target.cvegeo != 'all');
      var isSame = (source.cvegeo == target.cvegeo);
	  if(isOnetoOne && isSame){
		    $('.eod-config-traveltype-item[idref=true]').show();
			$('.eod-config-traveltype-item[idref=false]').show();
		  
		    $('.eod-config-traveltype-item.active').removeClass('active');
		    $('.eod-config-traveltype-item[idref=true]').addClass('active');
			$('.eod-config-traveltype-item[idref=false]').hide();
		  	obj.currentData.internals = true;
	   }else{
		   if(isOnetoOne && !isSame){
			   $('.eod-config-traveltype-item[idref=true]').show();
			   $('.eod-config-traveltype-item[idref=false]').show();
			   
			   $('.eod-config-traveltype-item.active').removeClass('active');
			   $('.eod-config-traveltype-item[idref=false]').addClass('active');
			   $('.eod-config-traveltype-item[idref=true]').hide();
			   obj.currentData.internals = false;
		   }else{
				$('.eod-config-traveltype-item[idref=true]').show();
				$('.eod-config-traveltype-item[idref=false]').show();
		   }
	   }*/

		if (banWeek && banSource && banTarget) {
			obj.prepareTheme();
		} else {
			obj.options.onThemeFail();
			obj.options.systemMessage('Los datos seleccionados no son válidos', {
				width: 240,
				height: 120,
				title: 'Información'
			});
		}

		//marcado de elementos erroneos
		if (!banTime) {

		}


	},
	updateUIPpal: function () {
		var obj = this;
		var cData = obj.currentData;
		var min = obj.getTimeInfo(cData.schedule.init).militia;
		var max = obj.getTimeInfo(cData.schedule.end).militia_end;
		
		$('#eod_config_init_time_select_container').html(min);
		$('#eod_config_end_time_select_container').html(max);
	},
	getGeoInfo:function(type,id){
		var obj = this;
		var list = obj.geoLists[type];
		var r = null;
		for(var x in list){
			if (list[x].cvegeo == id){
				r = list[x];
				break;
			}
		}
		return r;
	},
	updateHeader: function () {
		var obj = this;
		var cData = obj.currentData;

		var types = {
			ent: 'Todas las Entidades',
			mun: 'Todos los Municipios',
			dis: 'Todos los Distritos'
		};
		var dayTypes = {
			inner: 'Día entre semana',
			all: 'Toda la semana',
			sat: 'Día sábado'
		};

		var source = null;
		var target = null;

		if (cData.theme.originDestination) {
			source = cData.theme.origin;
			target = cData.theme.destinations;
		} else {
			source = cData.theme.destinations;
			target = cData.theme.origin;
		}

		if (cData.from.cvegeo != 'all' && $.isArray(source)) {
			source = source[0];
		}
		if (cData.to.cvegeo != 'all' && $.isArray(target)) {
			target = target[0];
		}
		
		var labelFrom = (cData.from.cvegeo == 'all') ? types[cData.from.type] +' de la ZMVM' :obj.getGeoInfo(cData.from.type,cData.from.cvegeo).nomgeo;
		var labelTo = (cData.to.cvegeo == 'all') ? types[cData.to.type] + ' de la ZMVM': obj.getGeoInfo(cData.to.type,cData.to.cvegeo).nomgeo;


		var cadena = 'Viajes de: ' + labelFrom + ' </br>Hacia: ' + labelTo;
		$('#eod_var_title').html(cadena);

		var labelSchedule = (cData.schedule.type == 'all') ? 'Durante todo el día' : 'Horario de ' + cData.schedule.init + ' hasta las ' + cData.schedule.end;
		labelSchedule = '<b>' + labelSchedule + '</b>';
		labelSchedule += '</br>' + dayTypes[cData.weekDay];

		$('#eod_subvar_title').html(labelSchedule);

		//imprime valores de tema en min max
	},
	//------------------Geo display---------------------------------------	
	gotoExtent: function (cvegeo) {
		var obj = this;
		var ds = obj.options.config.dataSources;

		var dataSource = $.extend(true, {}, ds.getExtent);
		dataSource.url += '/' + cvegeo;
		obj.getData(dataSource, {}, function (data) {
			if (data.response.success) {
				var extent = data.data.extent;
				obj.options.extent(extent);
			}
		});
	},
	spinner: function (option) {
		var obj = this;
		if (option == 'show') {
			if (!$('#eod_spinner_panel').attr('id')) {
				var w = obj.element.width();
				var h = obj.element.height();
				var cadena = '<div id="eod_spinner_panel" class="eod-spinner-panel" count="1" style="width:' + w + 'px;height:' + h + 'px">';
				cadena += '	<div class="ui-widget-overlay eod-block-overlay"></div>';
				cadena += '	<div class="eod-spinner-image-container"><span class="eod-spinner-image"></div>';
				cadena += '<div>';

				obj.element.append(cadena);
			} else {
				var count = parseInt($('#eod_spinner_panel').attr('count'), 10);
				$('#eod_spinner_panel').attr('count', count + 1);

			}
		} else {
			if ($('#eod_spinner_panel').attr('id')) {
				var count = parseInt($('#eod_spinner_panel').attr('count'), 10);
				if (count > 1) {
					$('#eod_spinner_panel').attr('count', count - 1);
				} else {
					$('#eod_spinner_panel').remove();
				}
			}
		}
	},

	//---------------------------------------------------------------------	
	getData: function (source, params, callback, error, before, complete) {
		var obj = this;
		if (source) {
			var silentMode = source.silentMode;

			var spinner = this.spinner;
			//Anexo de parametros que vengan definidos desde fuente de datos
			var s_params = source.params;
			var stringify = source.stringify;

			if (!(s_params === undefined)) {
				for (var x in s_params) { //anexo de la conifuracion del origen de datos
					params[x] = s_params[x];
				};
			}
			if (!(stringify === undefined) && stringify) {
				params = JSON.stringify(params);
			}
			//Estructura basica de peticion
			var dataObject = {
				data: params,
				success: function (json, estatus) {
					callback(json, estatus);
				},
				beforeSend: function (solicitudAJAX) {
					if(!silentMode)
						obj.spinner('show');
					if ($.isFunction(before)) {
						before(params);
					};
				},
				error: function (solicitudAJAX, errorDescripcion, errorExcepcion) {
					if ($.isFunction(error)) {
						error(errorDescripcion, errorExcepcion);
					};
				},
				complete: function (solicitudAJAX, estatus) {
					if(!silentMode)
						obj.spinner('hide');
					
					if ($.isFunction(complete)) {
						complete(solicitudAJAX, estatus);
					};
				}
			};
			//anexo de la conifuracion del origen de datos
			for (var x in source) {
				if (!(/field|name|id|params|stringify/.test(x))) dataObject[x] = source[x];
			};
			jQuery.support.cors = true;
			$.ajax(dataObject);
		}
	},
	getThemeParams:function(){
		var obj = this;
		var cData = obj.currentData;

		var week = cData.weekDay;
		var time = cData.schedule;
		var source = cData.from;
		var target = cData.to;
		var internals = cData.to;

		var dayweek = ['all', 'sat', 'inner']

		var initTime = (time.init) ? obj.getTimeInfo(time.init).militia : null;
		var endTime = (time.end) ? obj.getTimeInfo(time.end).militia_end : null;


		if (time.init == '00:00' && time.end == '23:59') {
			cData.schedule.type = 'all';
		} else {
			cData.schedule.type = 'range';
		}

		var params = {
			"dayOfWeek": dayweek.indexOf(week),
			"schedule": {
				"start": initTime,
				"end": endTime
			},
			"levelOrigin": source.type,
			"cveGeoOrigin": source.cvegeo,
			"levelDestination": target.type,
			"cveGeoDestination": target.cvegeo,
			"allDay": (time.type == 'all'),
			"addSame": cData.internals
		}	
		
		return params;
	},
	getDataStored:function(params){
		var obj = this;
		var data = obj.storedData;
		var r = null;
		for(var x in data){
			var item = data[x];
			if(item.params == JSON.stringify(params)){
				r = item;
				break;
			}
		}
		return r;
	},
	prepareTheme: function (func,options) {
		var obj = this;
		var silentMode = (options)?options.silentMode:null;
		var _params = (options)?options.params:null;
		
		if(silentMode === undefined)
			silentMode = false;
		
		var cData = obj.currentData;

		var week = cData.weekDay;
		var time = cData.schedule;
		var source = cData.from;
		var target = cData.to;
		var internals = cData.to;

		var dayweek = ['all', 'sat', 'inner']

		var initTime = (time.init) ? obj.getTimeInfo(time.init).militia : null;
		var endTime = (time.end) ? obj.getTimeInfo(time.end).militia_end : null;


		if (time.init == '00:00' && time.end == '23:59' && obj.forceBanAllDay) {
			cData.schedule.type = 'all';
		} else {
			cData.schedule.type = 'range';
		}

		var dataSource = $.extend(true, {}, obj.options.config.dataSources.theme);
			dataSource.silentMode = silentMode;
		var params = {
			"dayOfWeek": dayweek.indexOf(week),
			"schedule": {
				"start": initTime,
				"end": endTime
			},
			"levelOrigin": source.type,
			"cveGeoOrigin": source.cvegeo,
			"levelDestination": target.type,
			"cveGeoDestination": target.cvegeo,
			"allDay": (time.type == 'all'),
			"addSame": cData.internals
		}
		
		if(_params)
			params = _params;
		
		var onGetData = function(data){
			if(!silentMode){
				
				setTimeout(function(){
				
				obj.currentData.theme = data;
				obj.backupData = $.extend({}, true, obj.currentData);
				obj.updateHeader();
				obj.printConfig();
				data.internals = cData.internals;
				obj.options.onTheme(data);
					
				},200);
				
				
				//obj.printGraphData();
				//obj.gotoExtent(cData.geoSelected.join())
			}
			if ($.isFunction(func)) {
					func();
				}
			if(obj.storeDataService){  //se debe guardar datos
				if(obj.storedData.length >= 50)
					obj.storedData.shift();
				obj.storedData.push({params:JSON.stringify(params),result:data});
			}
		}
		
		var isDataStored = obj.getDataStored(params);
		if(isDataStored){
			onGetData(isDataStored.result);
		}else{
		
			obj.getData(dataSource, params, function (data) {

				obj.hasChanged = false;

				if (data.response.success) {
					onGetData(data.data);
				} else {
					if(!silentMode)
						obj.options.onThemeFail();
				}

			}, function () { //error
				if(!obj.silentServices)
					obj.options.onThemeFail();
				
			});
		}

	},
	prepareThemeBG: function (func,options) {
		var obj = this;
		var silentMode = true;
		var _params = (options)?options.params:null;
			params = _params;
		
		var onGetData = function(data){
			if(!silentMode){
				
					
					obj.currentData.theme = data;
					obj.backupData = $.extend({}, true, obj.currentData);
					obj.updateHeader();
					obj.printConfig();
					data.internals = cData.internals;
					obj.options.onTheme(data);

					if ($.isFunction(func)) {
						func();
					}
					
				
				
				//obj.printGraphData();
				//obj.gotoExtent(cData.geoSelected.join())
			}
			if(obj.storeDataService){  //se debe guardar datos
				if(obj.storedData.length >= 50)
					obj.storedData.shift();
				obj.storedData.push({params:JSON.stringify(params),result:data});
			}
		}
		
		var isDataStored = obj.getDataStored(params);
		if(isDataStored){
			onGetData(isDataStored.result);
		}else{
		
			obj.getData(dataSource, params, function (data) {

				obj.hasChanged = false;

				if (data.response.success) {
					onGetData(data.data);
				} else {
					if(!silentMode)
						obj.options.onThemeFail();
				}

			}, function () { //error
				if(!obj.silentServices)
					obj.options.onThemeFail();
			});
		}

	},
	clearThemeLayer: function () {
		var obj = this;
		var themeParams = {
			'LAYERS': ''
		}
		themeParams['MAPAESTATAL'] = 0; //(cData.geoType == 'edo')?idTheme:0;
		themeParams['MAPAMUNICIPAL'] = 0; //(cData.geoType == 'mun')?idTheme:0;;
		themeParams['MAPALOCALIDAD'] = 0;
		themeParams['MAPAAGEB'] = 0;
		themeParams['MAPADISTRITO'] = 0; //temp
		themeParams['MAPASECCION'] = 0;

		//obj.options.refreshMap(themeParams);
	},
	onClose: function () { //when destroy object
		var obj = this;
		var themeParams = {
			'LAYERS': 'd100,d101,d102,d109'
		}
		themeParams['MAPAESTATAL'] = 0;
		themeParams['MAPAMUNICIPAL'] = 0;
		themeParams['MAPALOCALIDAD'] = 0;
		themeParams['MAPAAGEB'] = 0;
		//obj.options.refreshMap(themeParams);
	},
	// Export
	exportStrats: function (type) {
		var obj = this;
		var data = obj.currentData;
		var theme = data.theme;
		if (theme) {
			var _var = data.varActive.descripcion;
			var detail = theme.detail;

			var columns = ['Estrato', 'Clave Geográfica', 'Nombre', 'Valor'];
			var values = [];

			for (var x in detail) {
				var item = detail[x];
				var geos = item.cvegeo;
				for (var y in geos) {
					var geo_item = geos[y];
					values.push([item.stratum, geo_item.cvegeo, geo_item.nombre, geo_item.indicador]);
				}
			}
			//Agregar fuente y variable tematizada
			var sourceCredits = {};

			var typeVar = obj.backupData.typeVarSelection;
			var postList = {
				'edomun': 'estatales, municipales y delegacionales',
				'edo': 'estatales',
				'mun': 'municipales y delegacionales',
			}


			values.push([]);
			values.push(['Fuente:' + obj.settings.source]);

			var ds = $.extend({}, true, obj.options.config.dataSources.exportData);
			var params = {
				title: 'Detalle de Estratos',
				columns: columns,
				values: values
			};
			obj.getData(ds, params, function (data) {
				if (data && data.response.success) {
					var url = ds.urlGet + '/' + type + '/' + data.data.id;
					window.location.assign(url);
				}
			});
			//obj.getData();
		}
	},
	openConfig: function () {
		var obj = this;
		obj.backupData = $.extend({}, obj.currentData);
		obj.element.attr('collapsed', 'false');
		setTimeout(function () {
			obj.options.detectCollision(obj.element);
		}, 1200);
		//carga arbol en posición
		//obj.loadTree(obj.currentData.index,true);
		obj.printConfig();
		obj.updateHeader();
	},
	cancelModify: function () {
		var obj = this;
		obj.hasChanged = false;
		obj.currentData = obj.backupData;
		obj.reloadTree();
		obj.printThemeDetail();
		obj.printGeoList(true); //just refresh
		obj.backupData = null;
		obj.closeConfig();
	},
	closeConfig: function () {
		var obj = this;
		obj.element.attr('collapsed', 'true');
		$('#eod_weekgraph_main_container').remove();
		clearTimeout(obj.timerPlayGraph);
		obj.timePlaying = false;
		obj.element.attr('expanded', 'false');
	},
	crateGraphData: function (data, title) {
		var obj = this;

		//obtieniendo colores
		var colors = obj.currentData.colors.colors;

		//Creando datos
		var listSerie = [{
			name: 'Estratos',
			colorByPoint: true,
			data: []
		}];
		var listDrillDown = {
			drillUpButton: {
				relativeTo: 'spacingBox',
				position: {
					y: 0,
					x: 0
				}
			},
			series: []
		}

		for (var x in data) {
			var sum = 0;
			var item = data[x];
			var id = item.stratum;

			//drillSerie
			var dserie = {
				name: 'Estrato ' + id,
				id: 'estrato' + id,
				data: []
			};

			for (var y in item.cvegeo) {
				var yitem = item.cvegeo[y];
				sum += parseInt(yitem.indicador, 10);
				dserie.data.push({
					name: yitem.cvegeo,
					label: yitem.nombre,
					y: parseInt(yitem.indicador, 10),
					drilldown: 'estrato' + id
				});
			}
			listDrillDown.series.push(dserie);

			listSerie[0].data.push({
				name: 'E' + id,
				label: 'E' + id,
				y: sum,
				drilldown: 'estrato' + id
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
			lang: {
				drillUpText: "Regresar a {series.name}"
			},

		});

		Highcharts.chart(obj.id + '_graph_strat', gobj);

	},
	formatMoney: function (nStr) {
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
	hexToRgb: function (hex) { //#FFFFFF
		var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
		return result ? {
			r: parseInt(result[1], 16),
			g: parseInt(result[2], 16),
			b: parseInt(result[3], 16)
		} : null;
	},
	openDialog: function (url, title) {
		var obj = this;

		if (!$('#eod-Info-dialog').attr('id')) {
			var cadena = '<div title="' + title + '" id="eod-Info-dialog"><div id="eod-html-doc"></div></div>';
			$('#panel-center').append(cadena);
			$('#eod-Info-dialog').dialog({
				dialogClass: 'eod-Info-dialogContainer',
				width: 620,
				height: 520,
				modal: true,
				close: function (event, ui) {
					$(this).dialog('destroy').remove();
				}
			});
		} else {
			$('#eod-Info-dialog').html('<div id="eod-html-doc"></div>').dialog();
		}

		$('#eod-html-doc').load(url);
	},

	//Control de ficha *********************************************************************************************************************
	onIdentify: function (identifyData) {
		var obj = this;
		var pos = identifyData.pos;
		var cData = obj.currentData;
		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true, {}, ds.infoPoint);

		var types = [null, 'distrito', 'seccion'];
		var params = {
			cvegeo: cData.geoSelected[0],
			level: types[parseInt(cData.geoType)],
			point: 'POINT(' + pos.lon + ' ' + pos.lat + ')'
		}
		obj.getData(dataSource, params, function (data) {
			if (data && data.data && data.data.data) {
				obj.getCardData(data.data.data, function (data) {
					obj.printCardData(data);
				});
			} else {
				obj.options.onIdentifyFail(identifyData.pos);
			}
		});
	},
	getCardData: function (geo, func) {
		var obj = this;
		var cData = obj.currentData;

		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true, {}, ds.getCardValues);

		var params = {
			id: geo.cvegeo,
			level: cData.geoType
		}
		obj.getData(dataSource, params, function (data) {
			if (data.data) {
				data.data.geoName = geo.Nombre;
				func(data.data);
			}
		});
	},

	printCardData: function (data) {
		var obj = this;
		var cdata = obj.currentData;
		var dataExport = [];

		var geoType = ['Distrito', 'Seccion'];
		geoType = geoType[parseInt(cdata.geoType) - 1];

		var dialogTitle = data.geoName;

		if ($('#tooleod_info').attr('id'))
			$('#tooleod_info').remove();


		var cadena = '<div id="tooleod_info" title="' + dialogTitle + '"><div id="tooleod_info_content"></div></div>';
		$("#panel-center").append(cadena);

		$('#tooleod_info').dialog({
			resizable: false,
			width: 800,
			modal: true,
			height: 550,
			buttons: {
				"Cerrar": function () {
					$(this).dialog("close");
				}
			},
			close: function () {
				obj.options.onCloseIdentify()
			}
		});
		var resume = data.general;
		var list = data.indicators;

		cadena = '<div id="eod_card_header"></div>';
		cadena += '<div class="eod-card-title-indicators">Indicadores</div>';
		cadena += '<div id="eod_card_indicators_container" class="eod-animated">';
		cadena += '	<div id="eod_card_indicators_closebtn"><span class="sprite-eod-sleft"></span><span class="sprite-eod-sright"></span></div>';
		cadena += '	<div id="eod_card_indicators"></div>';
		cadena += '</div>';
		cadena += '<div id="eod_card_values" class="eod-animated"></div>';
		cadena += '<div id="eod_card_toolbar"></div>';

		$('#tooleod_info_content').html(cadena);


		//encabezado
		cadena = '<table class="eod-table-color">';

		for (var x in resume) {
			var item = resume[x];
			cadena += '<tr><td>' + item.label + '</td><td>' + item.value + '</td></tr>';
			dataExport.push([item.label, item.value]);
		}
		cadena += '</tbody></table>';
		dataExport.push(['', '']);
		$('#eod_card_header').html(cadena);

		//menú lateral
		var count = 1;
		cadena = '<ul>';
		for (var x in list) {
			var item = list[x];
			cadena += '<li class="' + ((x == 0) ? 'active ' : '') + 'geoelectoral-card-var eod-animated" pos="' + x + '"><div idref="' + item.field + '" class="ui-icon ui-icon-info eod-card-varid"></div><label>' + item.label + '</label></li>';
			count++;
		}
		cadena += '</ul>'
		$('#eod_card_indicators').html(cadena);

		$('.eod-card-varid').each(function () {
			$(this).click(function () {
				var idref = $(this).attr('idref');
				obj.openMetadataVar(idref);
			});
		});

		//Contenido desglozado
		var indOrder = obj.options.config.config.settings.cardIndicatorOrder;

		cadena = '';
		count = 1;
		dataExport.push(['Indicadores', '']);
		for (var x in list) {
			var item = list[x];
			cadena += '<div id="geoelectoral_ind_' + x + '" class="eod-card-block"><div pos="' + x + '" class="eod-card-title">' + count + '. ' + item.label + '</div>';
			dataExport.push([item.label]);
			cadena += '<div class="eod-card-blockcontent">';
			cadena += '<table class="eod-table-color"><tbody>';

			var head = [];
			cadena += '<thead><tr>';
			var _count = 0;
			for (var y in indOrder) {
				var _item = indOrder[y];
				cadena += '<td ' + ((_count == 0) ? 'class="geolectoral-card-table-labelcolumn"' : '') + '>' + _item.label + '</td>';
				head.push(_item.label);
				_count++;
			}
			dataExport.push(head);

			cadena += '</tr></thead>';

			cadena += '<tbody>';
			for (var y in item.value) { //extrae fila
				var _item = item.value[y];

				var content = [];
				cadena += '<tr>';
				//impresion de columnas
				for (var z in indOrder) {
					var zitem = indOrder[z];
					var elem = _item[zitem.id];

					content.push(elem);

					var alignNumber = '';
					if ($.isNumeric(elem)) {
						alignNumber = 'style="text-align:right"';
						if ((elem).isInteger()) {
							elem = elem.format();
						} else {
							elem = elem.format(2);
						}
					}
					cadena += '<td ' + alignNumber + '>' + elem + '</td>';

				}
				cadena += '</tr>';
				dataExport.push(content);

			}
			cadena += '</tbody></table>';
			cadena += '</div></div>';
			dataExport.push(['']);
			count++;
		}
		$('#eod_card_values').html(cadena);


		//Exportación
		var exportList = obj.settings.exportTypes;
		cadena = '';
		for (var x in exportList) {
			cadena += '<div idref="' + exportList[x] + '" class="eod-card-export-type-item sprite-eod-doc-' + exportList[x] + '"></div>';
		}
		//inclusión de descarga nacional
		var doc = obj.options.config.config.settings.cardIndicatorNalDownload;
		var docPDF = obj.options.config.config.settings.cardIndicatorNalDownloadPDF;
		cadena += '<div class="eod-card-export-link-item"><label>Nacional </label><a href="' + doc + '" target="_blank"><span class="sprite-eod-doc-xls"></span></a><a href="' + docPDF + '" target="_blank"><span class="sprite-eod-doc-pdf-mini"></span></a></div>';

		cadena += '</div>';

		$('#eod_card_toolbar').html(cadena);

		$('.eod-card-export-type-item').each(function () {
			$(this).click(function () {
				var idref = $(this).attr('idref');
				obj.exportIndicators(idref, dataExport);
			});

		});



		obj.setCardEvents();
	},
	exportIndicators: function (type, edata) {
		var obj = this;

		edata.push(['']);
		edata.push(['Fuente:' + obj.settings.source]);

		var cdata = obj.currentData;

		var ds = $.extend({}, true, obj.options.config.dataSources.exportData);
		var params = {
			title: 'Información Geoelectoral',
			columns: ['', ''],
			values: edata
		};
		obj.getData(ds, params, function (data) {
			if (data && data.response.success) {
				var url = ds.urlGet + '/' + type + '/' + data.data.id;
				window.location.assign(url);
			}
		});
		//obj.getData();
	},
	setCardEvents: function () {
		var obj = this;
		var scrollpos = [];
		var count = 0;
		var fix = 0;

		var jumpScrollToVar = function (pos) {
			var pos = (pos === undefined) ? $('.geoelectoral-card-var.active').attr('pos') : pos;
			var top = scrollpos[parseInt(pos)];
			$('#eod_card_values').animate({
				scrollTop: top
			}, 500, 'swing', function () {});
		}
		$('.eod-card-title').each(function () {
			var top = $(this).position().top;
			if (count == 0)
				fix = top;
			scrollpos.push(top - fix);
			count++;
		});

		$('#eod_card_values').scroll(function () {
			var scroll = $('#eod_card_values').scrollTop();
			for (var x in scrollpos) {
				if ((scrollpos[x] - scroll) >= 0) {
					if ((scrollpos[x] - scroll) > $('#eod_card_values').height())
						x--;

					$('.geoelectoral-card-var.active').removeClass('active');
					$('.geoelectoral-card-var[pos=' + x + ']').addClass('active');
					break;
				}
			}
		});
		$('.geoelectoral-card-var').each(function () {
			$(this).unbind('click');
			$(this).click(function () {
				var pos = $(this).attr('pos');

				jumpScrollToVar(pos);

				/*
				var top = scrollpos[parseInt(pos)];
				$('#eod_card_values').animate({scrollTop:top}, 500, 'swing', function() { 
				});
				*/

				//$('#eod_card_values').scrollTop(top);
			})
		});

		$('#eod_card_indicators_closebtn').unbind('click');
		$('#eod_card_indicators_closebtn').click(function () {
			var status = $('#tooleod_info_content').attr('status');
			if (status != 'collapsed') {
				$('#tooleod_info_content').attr('status', 'collapsed');
			} else {
				$('#tooleod_info_content').attr('status', 'expanded');
			}
			setTimeout(function () {
				obj.setCardEvents();
				$('.geoelectoral-card-var.active').click();
			}, 1200);

		});

	},
	printTimeSelectors: function (func) {
		var obj = this;
		var cData = obj.currentData;
		
		if(cData.schedule.end == '24:00')
			cData.schedule.end = '23:59';
		
		var min = (cData.schedule.init) ? cData.schedule.init : '00:00';
		var max = (cData.schedule.end ) ? cData.schedule.end : '23:59';

		
		
		var setEvents = function(){
			var init = $('#eod_config_init_time_select option:selected').val();
			var end = $('#eod_config_end_time_select option:selected').val();

			
			
			cData.schedule.init = init;
			cData.schedule.end = end;
			
			obj.checkSelectionIntegrity();
			obj.updateUIPpal();
			//$('#eod_table_time_end').html(cData.schedule.end);
			obj.triggerChanges = false;
			
			
			var chart = $('#eod_weekgraph_container').highcharts();

			var min = cData.schedule.init;
			min = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.init).militia) + ':00');
			var max = cData.schedule.end;
			max = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.end).militia) + ':00');
			chart.xAxis[0].setExtremes(min, max);
			
			obj.printTimeSelectors(func);
			//$('#eod_config_schedule_btn').removeClass('active');

		}
		
		var init_timeList = obj.getTimeAbove(min);
		var cadena = '<select id="eod_config_init_time_select" class="eod-config-init-time-select">';
		for (var x in init_timeList) {
			var item = init_timeList[x];
			var selected = (item.militia == min) ? ' selected = "selected"' : '';
			var disabled = (item.disabled) ? ' disabled = "disabled"' : '';
			cadena += '	<option value="' + item.militia + '" ' + disabled + ' ' + selected + '>' + item.militia + ' hrs.</option>';
		}
		cadena += '</select>';

		$('#eod_table_time_init').html(cadena);

		var end_timeList = obj.getTimeBelow(min);
		var cadena = '<select id="eod_config_end_time_select" class="eod-config-end-time-select">';
		for (var x in end_timeList) {
			var item = end_timeList[x];
			var disabled = (item.disabled) ? ' disabled = "disabled"' : '';
			var selected = (item.militia == max) ? ' selected = "selected"' : '';
			cadena += '	<option value="' + item.militia + '" ' + disabled + ' ' + selected + '>' + item.militia_end + 'hrs. ' + item.diff + '</option>';
		}
		cadena += '</select>';

		$('#eod_table_time_end').html(cadena);

		//$('#eod_config_end_time_select_container').html(max);

		$('#eod_config_init_time_select').change(function () {
			obj.forceBanAllDay = false;
			var init = $('#eod_config_init_time_select option:selected').val();
			var end = $('#eod_config_end_time_select option:selected').val();

			
			setEvents();
			
		})
		$('#eod_config_end_time_select').change(function () {
			obj.forceBanAllDay = false;
			var init = $('#eod_config_init_time_select option:selected').val();
			var end = $('#eod_config_end_time_select option:selected').val();
			
			setEvents();

			
			
		})
		
		

	},
	//Grafica con periodo
	createPeriodGraph: function (container, data) {
		var obj = this;
		var cData = obj.currentData;
		//determina si al modificar la grafica se disparara la tematizacion, util para refrescar grafica sin recrear tema


		var setRange = function () {
			var chart = $('#' + container).highcharts();

			var min = cData.schedule.init;
			min = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.init).militia) + ':00');
			var max = cData.schedule.end;
			max = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.end).militia) + ':00');
			chart.xAxis[0].setExtremes(min, max);
		}

		var printLocalTime = function () {

			var min = (cData.schedule.init) ? cData.schedule.init : '00:00';
			var max = (cData.schedule.end) ? cData.schedule.end : '23:59';
			
			obj.updateUIPpal();

			$('#eod_config_schedule_btn').removeClass('active');


			obj.printTimeSelectors();


		}
		var updateGraphNavigator = function () {
			obj.triggerChanges = false;
			var _min = cData.schedule.init;
			var _max = cData.schedule.end;
			var chart = $('#' + container).highcharts();

			_max = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.end).militia) + ':00');
			_min = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.init).militia) + ':00');
			chart.xAxis[0].setExtremes(_min, _max);

		}
		var updateTimes = function (values) {
			var min = values.min;
			var max = values.max;
			if (min && max) {

				cData.schedule.type = 'range';
				cData.schedule.init = min.militia;
				cData.schedule.end = max.militia;

				obj.checkSelectionIntegrity();

				var chart = $('#' + container).highcharts();
				var _min = cData.schedule.init;
				var _max = cData.schedule.end;

				if (_min != min.militia || _max != max.militia) {
					_max = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.end).militia) + ':00');
					_min = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.init).militia) + ':00');
					chart.xAxis[0].setExtremes(min, max);
				}

				printLocalTime();
			}

		}
		var timeTrigger = 0; 
		var triggerChage = function (values) { //dispara cambios despues de 300 milisegundos de efectuar la modificación de la grafica
			obj.forceBanAllDay = false;
			clearTimeout(timeTrigger);
			timeTrigger = setTimeout(function () {
				var min = obj.getNearTime(values.min);
				var max = obj.getNearTime(values.max);

				if (min != '')
					updateTimes({
						min: min,
						max: max
					});
			}, 500);
		}
		Highcharts.setOptions({
			global: {
				timezoneOffset: 360,
				useUTC: true
			}
		});
		//container = ''
		Highcharts.stockChart(container, {
			scrollbar: {

				liveRedraw: false
			},
			rangeSelector: {
				inputEnabled: false,
				buttonTheme: {
					visibility: 'hidden'
				},
				labelStyle: {
					visibility: 'hidden'
				},
			},
			navigator: {
				xAxis: {
					visible: false,
					minRange: 1000
				}
			},
			exporting: {
				enabled: false
			},
			chart: {
				renderTo: 'chart1_container',
				type: 'spline'
			},
			plotOptions: {
				line: {
					dataLabels: {
						enabled: true
					},
					enableMouseTracking: true
				}
			},
			subtitle: {
				text: ''
			},
			title: {
				text: ''
			},
			tooltip: {
				enabled: true,
				formatter: function () {
					var s = [];
					$.each(this.points, function (i, point) {
						s.push('<span style="color:' + point.series.color + ';font-weight:bold;">' +
							'Hora: ' + Highcharts.dateFormat('%H:%M', this.x) + ' - '+ Highcharts.dateFormat('%H:59', this.x)+' <br/>' +
							//'Hora: '+ Highcharts.dateFormat('%I:%M %p', this.x)+' <br/>'+
							point.series.name + ' : ' + point.y + '<span>');
					});
					return s.join(' and ');
				},
				shared: true
			},
			xAxis: {
				startOnTick: true,
				endOnTick: true,
				maxPadding: 0.02,
				//minorTickPosition:'outside',
				minRange: 15 * 60 * 1000,
				tickInterval: 60 * 60 * 1000, //intervalos de horas
				dateTimeLabelFormats: {
					//hour:'%I %p',
					hour: '%H:%M',
					day: '%H:%M'
				},
				type: 'datetime',
				events: {
					setExtremes: function (e) {

						var min = Highcharts.dateFormat('%H:%M', e.min);
						var max = Highcharts.dateFormat('%H:%M', e.max);
						if (obj.triggerChanges && min != '' && max != '') {
							triggerChage({
								min: min,
								max: max
							});
						}

						obj.triggerChanges = true;
					}
				}
			},
			yAxis: {
				title: {
					text: 'Viajes'
				},
				tickInterval:5000,
				showLastLabel:true,
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
			series: [{
				data: data,
				name: 'Viajes'
			}],
			events: {
				selection: function (event) {
					// log the min and max of the primary, datetime x-axis
					/*console.log(
						Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', event.xAxis[0].min),
						Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', event.xAxis[0].max)
					);*/
					// log the min and max of the y axis
					//console.log(event.yAxis[0].min, event.yAxis[0].max);
				}
			},
			credits: {
				enabled: false
			}
		});

		setTimeout(function () {
			updateGraphNavigator();
		}, 500);

		printLocalTime();



		/*
			$('#eod_table_time_init').unbind('click').click(function(){
				obj.createWatch(cData.schedule.init,'init',function(time){
					cData.schedule.init = time.hours+':'+time.mins+' '+time.merid;
					obj.checkSelectionIntegrity();
					var ttime = obj.getTimeInfo(cData.schedule.init);
					$('#eod_table_time_init').html(cData.schedule.init);
					setRange();
				}); 
			});*/
		/*$('#eod_table_time_end').unbind('click').click(function(){
			obj.createWatch(cData.schedule.end,'end',function(time){
				cData.schedule.end = time.hours+':'+time.mins+' '+time.merid;
				if(cData.schedule.end == '12:00 pm')
					cData.schedule.end = '11:59 pm';
				
				obj.checkSelectionIntegrity();
				$('#eod_table_time_end').html(cData.schedule.end);
				setRange();
			}); 
		});*/


		/*var chart = $('#'+container).highcharts();
		   setTimeout(function(){
				var min = Date.parse('01/01/2018 09:05:30');
				var max = Date.parse('01/01/2018 13:05:30');
				
				chart.xAxis[0].setExtremes(min,max);
		   },2000);*/
	},
	// Creacion de reloj de seleccion de hora
	showGraphWeek: function () {
		var obj = this;
		var cData = obj.currentData;
		obj.element.attr('expanded', 'true');

		//sprite-eod-close
		if ($('#eod_weekgraph_container').attr('id')) $('#eod_weekgraph_container').remove();
		var w = $('#eod_geo_container').width();
		var h = $('#eod_geo_container').height();

		//agrega contenedor a interfaz
		var cadena = '<div id="eod_weekgraph_main_container" class="eod-weekgraph-main-container" playing="false">';
		cadena += '	<div id="eod_weekgraph_container" class="eod-weekgraph-container"></div>';
		cadena += '  <div id="eod_weekgraph_close"><span class="sprite-eod-backm"></span></div>';
		cadena += '  <div id="eod_weekgraph_bottom">';
		cadena += '  <table align="center" with="600" class="eod-table-hours" cellspacing="2">';
		cadena += '  		<tr style=""><td colspan="3" id="eod_weekgraph_playcontrols_container">';
		cadena += '  		</tr></td>';
		cadena += '  		<tr><td width="200" height="32">Hora inicio</td><td  width="200">Hora fin</td><td id="eod_table_time_play" status="' + ((!obj.timePlaying) ? 'play' : 'pause') + '"><div class="sprite-eod-play"></div><div class="sprite-eod-pause"></div></td></tr>';
		cadena += '  		<tr><td id="eod_table_time_init" height="32" class="eod-table-hour-display">--:--</td><td id="eod_table_time_end" class="eod-table-hour-display">--:--</td><td id="eod_table_time_all" class="eod-config-selectable ' + ((obj.playAll) ? 'active' : '') + '">Acumulado</td></tr>';
		cadena += '	</table>';
		cadena += '	<div class="eod-weekgraph-hidecontrols01"></div>';
		cadena += '	</div>';
		cadena += '</div>';

		$('#eod_geo_container').append(cadena);
		
		obj.updatePlayControls();
		

		$('#eod_weekgraph_close').click(function () {
			$('#eod_weekgraph_main_container').remove();
			obj.printUIConfig();
			clearTimeout(obj.timerPlayGraph);
			obj.timePlaying = false;
			obj.element.attr('expanded', 'false');
		});
		$('#eod_table_time_play').click(function () {
			obj.timePlaying = !obj.timePlaying;
			$(this).attr('status', ((!obj.timePlaying) ? 'play' : 'pause'));
			$('#eod_weekgraph_main_container').attr('playing', obj.timePlaying);

			if (obj.timePlaying) {
				obj.playGraphTime();
			} else {
				//obj.currentData.schedule.type
				clearTimeout(obj.timerPlayGraph);
				obj.printTimeSelectors();
			}
			obj.updatePlayControls();
		});

		$('#eod_table_time_all').click(function () {
			obj.playAll = !obj.playAll;
			if (obj.playAll) {
				$(this).addClass('active');
			} else {
				$(this).removeClass('active');
			}
			obj.updatePlayControls();
		});




		var container = $('#eod_weekgraph_container');


		var cData = obj.currentData;
		var week = cData.weekDay;
		var time = cData.schedule;
		var source = cData.from;
		var target = cData.to;

		var dayweek = ['all', 'sat', 'inner']

		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true, {}, ds.getTravels);
		var params = {
			"dayOfWeek": dayweek.indexOf(week),
			"levelOrigin": source.type,
			"cveGeoOrigin": source.cvegeo,
			"levelDestination": target.type,
			"cveGeoDestination": target.cvegeo,
			"allDay": (time.type == 'all')
		}
		obj.getData(dataSource, params, function (data) {
			if (data.response.success) {
				var dserie = [];
				var list = data.data.travels;
				/* 
				 var extra = list[0];
				 extra.start = '23:45';
				 extra.end = '23:59';
				 
				 list.push(extra);
				 */



				for (var x in list) {
					var item = list[x];
					dserie.push([Date.parse('01/01/2018 ' + item.start + ':00'), item.travels, ]);
				}
				obj.createPeriodGraph('eod_weekgraph_container', dserie);
			}
		});
	},
	createWatch: function (time, type, func) {
		var thours = '00';
		var tmins = '00';
		var tmerid = 'am';
		if (time != null) {
			var vtime = time.split(':');
			thours = vtime[0];
			tmins = vtime[1].split(' ')[0];
			tmerid = vtime[1].split(' ')[1];
		}
		if (type == 'end' && thours == '11' && tmins == '59' && tmerid == 'pm') {
			thours = '00';
			tmins = '00';
			tmerid = 'am';
		}

		var obj = this;
		if ($('#eod_watch_container').attr('id')) $('#eod_watch_container').remove();
		var w = $('#eod_geo_container').width();
		var h = $('#eod_geo_container').height();
		//agrega contenedor a interfaz
		var cadena = '<div id="eod_watch_container" class="eod-watch-container" style="width:' + w + 'px;height:' + h + 'px">';
		cadena += '  <div id="eod_watch_close" class=""><span class="sprite-eod-close"></span></div>';
		cadena += '	<div id="eod_watch_circle" class="eod-watch-circle"></div>';
		cadena += '	<div id="eod_watch_display" class="eod-watch-display"></div>';
		cadena += '</div>';

		$('#eod_geo_container').append(cadena);
		$('#eod_watch_close').click(function () {
			$('#eod_watch_container').remove();
		});


		var container = $('#eod_watch_container');

		var refreshDisplay = function () {
			var cadena = '<label>' + thours + ':' + tmins + '</label> <label idref="' + tmerid + '" id="eod_watch_display_meridian">' + tmerid + '</label>';
			$('#eod_watch_display').html(cadena).unbind('click').click(function () {
				tmerid = (tmerid == 'am') ? 'pm' : 'am';
				refreshDisplay();
			});
		}

		var printMinutes = function () {
			var cadena = '';
			cadena = '';
			var circle = $('#eod_watch_circle');
			var mins = ['00', '14', '29', '45'];
			for (var x in mins) {
				var item = mins[x];
				var active = (item == tmins) ? 'active' : '';
				cadena += '<div class="eod-watch-min eod-watch-min-' + item + ' ' + active + '" time="' + item + '">' + item + '</div>';
			}
			circle.html(cadena);

			$('.eod-watch-min').each(function () {
				$(this).click(function () {
					tmins = $(this).attr('time');
					if ($.isFunction(func)) {
						if (thours == '12' && tmerid == 'am') {
							thours = '00';
						}
						if (type == 'end' && thours == '00' && tmins == '00' && tmerid == 'am') {
							thours = '11';
							tmins = '59';
							tmerid = 'pm';
						}
						thours = (parseInt(thours) < 10) ? '0' + parseInt(thours) : thours;
						func({
							hours: thours,
							mins: tmins,
							merid: tmerid
						});
					}

					$('#eod_watch_close').click();
				});
			})
		}

		var printHours = function () {
			var cadena = '';
			cadena = '';
			var circle = $('#eod_watch_circle');
			var hours = ['12', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'];
			for (var x in hours) {
				var item = hours[x];
				var active = (parseInt(item) == parseInt(thours)) ? 'active' : '';
				cadena += '<div class="eod-watch-hour eod-watch-hour-' + item + ' ' + active + '"  time="' + item + '">' + item + '</div>';
			}
			circle.html(cadena);

			$('.eod-watch-hour').each(function () {
				$(this).click(function () {
					thours = $(this).attr('time');
					refreshDisplay();
					printMinutes();
				});
			})

		}();

		refreshDisplay();


	},
	printConfig: function () {
		var obj = this;
		var cData = obj.currentData;
		var list = cData.theme;
		var sort = obj.mainGraphSortBy;
		var reverse = obj.mainGraphSortReverse;

		var sortName = function compareName(a, b) {
			if (a.name < b.name)
				return -1;
			if (a.name > b.name)
				return 1;
			return 0;
		}

		var sortValue = function compareValue(a, b) {
			if (a.value < b.value)
				return -1;
			if (a.value > b.value)
				return 1;
			return 0;
		}

		//crea valores de series
		var series = [{
			name: 'test',
			data: []
		}];
		var categories = [];
		var vlist = [];
		var others = list.others;
		if (list && list.destinations) {
			list = list.destinations;
			for (var x in list) {
				var item = list[x];
				if (item.travels > 0)
					vlist.push({
						name: item.name,
						value: item.travels,
						pos: x
					});
			}
		}
		
		if (vlist.length > 0 || others.length > 0 ) {

			//estructura para grafica y botones de ordenado
			var cadena = '';
			cadena += '<div id="eod_graph_container" class="eod-graph-container"></div>';
			cadena += '<div id="eod_graph_bottom_title" class="eod-graph-container-bottom-title"></div>';
			cadena += '<div id="eod_graph_tools" class="eod-graph-tools">';
			cadena += '	   <div id="eod_sort_byalpha" idref="name" class="eod-sort-option ' + ((sort == 'name') ? 'active' : '') + '" reverse="' + ((reverse.name)) + '">';
			cadena += '	   		<div id="eod_sort_byalphan" reverse="false" class="eod-sort-graph sprite-eod-sort-alpha"></div>';
			cadena += '	   		<div id="eod_sort_byalphar" reverse="true" class="eod-sort-graph sprite-eod-sort-alphar"></div>';
			cadena += '   </div>';
			cadena += '	   <div id="eod_sort_bynum" idref="value" class="eod-sort-option ' + ((sort == 'value') ? 'active' : '') + '" reverse="' + ((reverse.value)) + '" >';
			cadena += '	   		<div id="eod_sort_bynumn" reverse="false" class="eod-sort-graph sprite-eod-sort-num"></div>';
			cadena += '	   		<div id="eod_sort_bynumr" reverse="true" class="eod-sort-graph sprite-eod-sort-numr"></div>';
			cadena += '	   </div>';
			cadena += '</div>';

			$('#eod_conf_container').html(cadena);
			$('.eod-sort-option').each(function () {
				$(this).click(function () {
					var idref = $(this).attr('idref');
					if ($(this).hasClass('active')) {
						reverse[idref] = !reverse[idref];
					}
					obj.mainGraphSortBy = idref;
					obj.printConfig();
				});

			});


			//ordenamiento
			if (sort == 'name') {
				vlist.sort(sortName);
			} else {
				vlist.sort(sortValue);
			}

			//reverse values if neccesary
			if (reverse[sort])
				vlist.reverse();
			
			
			
			
			//inclusion de otros valores
			var isSourceAll = (cData.from.cvegeo == 'all');
			var isTargetAll  = (cData.to.cvegeo == 'all');
			
			var xLabel = '';
			
			if (others && others.length > 0) {
				for (var x in others) {
					var item = others[x];
					if (item.travels > 0){
						if(item.name=='Viajes en horarios no especificados'){
							xLabel = '';//'La suma de los parciales puede no coincidir con el total ya que se tienen viajes con horario de inicio o termino -no especificado-.';
						}else{
							//reglas especiales basadas en el tipo de cardinalidad del viaje
							if(item.name == 'No especificado' && !isSourceAll && isTargetAll){
								item.name = 'Destino de viaje no especificado';
							}
							if(item.name == 'No especificado' && isSourceAll && !isTargetAll){
								item.name = 'Origen de viaje no especificado';
							}

							if(item.name == 'Fuera de Zona Metropolitana' && !isSourceAll && isTargetAll){
								item.name = 'Destino de viaje fuera de la zona metropolitana';
							}
							if(item.name == 'Fuera de Zona Metropolitana' && isSourceAll && !isTargetAll){
								item.name = 'Origen de viaje fuera de la zona metropolitana';
							}

							vlist.push({
								name: item.name,
								value: item.travels,
								pos: x + list.length
							});
						}
					}
				}
			}
			//------------------------------------------------------------------------------------
			for (var x in vlist) {
				var item = vlist[x];
				categories.push(item.name);
				series[0].data.push(item.value);
			}
			$('#eod_graph_bottom_title').html(xLabel);
			obj.createGraph({
				cats: categories,
				series: series,
				xtitle:xLabel
			});


		} else {
			//si no hay valores para graficar coloca mensaje
			var cadena = '';
			cadena += '<div id="eod_graph_msg_container" class="eod-graph-msg-container">Total de viajes: 0</div>';
			$('#eod_conf_container').html(cadena);
		}



	},
	updatePlayControls:function(){
		var obj = this;
		var cData = obj.currentData;
		
		$('#eod_table_time_play').attr('status', ((!obj.timePlaying) ? 'play' : 'pause'));
		$('#eod_weekgraph_main_container').attr('playing',obj.timePlaying);
		
		//casillas de carga
		var hours = [];
			for (var x = 0; x < 24; x++) {
				var h = ((x < 10) ? '0' + x : x) + ':00';
				var _h = ((x < 9) ? '0' + (x + 1) : (x + 1)) + ':00';
				hours.push({
					init: h,
					end: _h
				});
			}
		//tabla de reproduccion
		//revisa si el tiempo ya ha sido precacheado
		var checkIfCached = function(init,end){
			if(obj.playAll)init = '00:00';
			
			var initTime = (init) ? obj.getTimeInfo(init).militia : null;
			var endTime = (end) ? obj.getTimeInfo(end).militia_end : null;
			var r = false;
			if(initTime && endTime){
				var params = obj.getThemeParams();
					params.allDay = false;
					params.schedule.start = initTime;
					params.schedule.end = endTime;
				var isDataStored = obj.getDataStored(params);
				if(isDataStored){
					r = true;	
				}
			}
			return r;
		}
		var _isActive = function(init, end){
			var isFullHour = (cData.schedule.end.split(':')[1] == '00');
			var posIndex = (obj.getTimeInfo(cData.schedule.end).position);
			var posEnd = (obj.getTimeInfo(end).position);
			var r = (((obj.playAll)?'00:00':init) == cData.schedule.init && end == cData.schedule.end);
			if(!r){
				if(obj.playAll && isFullHour && (posEnd < posIndex))
					r = true;
			}
			return r;
		}
		//eod_weekgraph_playcontrols_container
		var cadena = '<div class="eod-graph-table-control-time">';
		//var cadena = '<table class="eod-graph-table-control-time eod-animated" cellspacing="0" cellpadding="0"><tr>';
		for(var x in hours){
			var item = hours[x];
			var t = parseInt(x);
			var hour = ((t) < 10)?'0'+(t):t;
			var isCached = checkIfCached(item.init,item.end);
			
			var isActive = _isActive(item.init,item.end);//(((obj.playAll)?'00:00':item.init) == cData.schedule.init && item.end == cData.schedule.end);
			
			//cadena+= '<td pos="'+x+'" tinit="'+item.init+'" tend="'+item.end+'" cached="'+(isCached)+'" iscurrent="'+isActive+'">'+hour+'</td>';
			cadena+='<div pos="'+x+'" tinit="'+item.init+'" tend="'+item.end+'" cached="'+(isCached)+'" iscurrent="'+isActive+'">'+hour+'</div>';
		}
		//cadena+= '</tr></tabel>';
		cadena+='</div>';
		$('#eod_weekgraph_playcontrols_container').html(cadena);
		
		
		$('.eod-graph-table-control-time div').each(function(){
			$(this).click(function(){
				
				
				var init = $(this).attr('tinit');
				var end = $(this).attr('tend');
				
				
				if (obj.playAll) {
					init = '00:00';
				}

				cData.schedule.init = init;
				cData.schedule.end = end;

				var onGetData = function(){
					var chart = $('#eod_weekgraph_container').highcharts();
					min = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.init).militia) + ':00');
					max = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.end).militia) + ':00');
					obj.triggerChanges = false;
					chart.xAxis[0].setExtremes(min, max);

					//
					obj.printTimeSelectors();
					setTimeout(function(){
						obj.updatePlayControls();	
					},100);
					
					if(obj.timePlaying){
						$('#eod_table_time_play').click();
					}
					
				}

				obj.prepareTheme(function () {
					onGetData();
				})
				
				
			});
		})
		
		
		
	},
	playGraphTime: function (chart) {
		var obj = this;
		var cData = obj.currentData;
		var container = 'eod_weekgraph_container';
		if ($('#' + container).attr('id')) {
			var chart = $('#' + container).highcharts();

			var playing = obj.timePlaying;
			var playAll = obj.playAll;

			var hours = [];
			for (var x = 0; x < 24; x++) {
				var h = ((x < 10) ? '0' + x : x) + ':00';
				var _h = ((x < 9) ? '0' + (x + 1) : (x + 1)) + ':00';
				hours.push({
					init: h,
					end: _h
				});
			}
			var preloadThemes = function(pos){
				if(pos === undefined)
					pos = 1;
				
				if (pos < 24 && obj.timePlaying) {
						var time = hours[pos];
						if (playAll) {
							time.init = '00:00';
						}

						var params = obj.getThemeParams();
							params.allDay = false;
							params.schedule.start = time.init;
							params.schedule.end = time.end;
							

						params.schedule.start = (time.init) ? obj.getTimeInfo(time.init).militia : null;
						params.schedule.end = (time.end) ? obj.getTimeInfo(time.end).militia_end : null;

						var options = {silentMode:true,params:params};

						obj.prepareTheme(function() {
							pos++;
							preloadThemes(pos);
							
							setTimeout(function(){
								obj.updatePlayControls();	
							},100);
							
							
						},options); //true silent mode
				}

			}

			var play = function (pos) {
				if (pos < hours.length && playing) {
					 //inicia la carga de temas en segundo plano
					
					var time = hours[pos];
					if (playAll) {
						time.init = '00:00';
					}

					cData.schedule.init = time.init;
					cData.schedule.end = time.end;
					
					var onGetData = function(){
						min = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.init).militia) + ':00');
						max = Date.parse('01/01/2018 ' + (obj.getTimeInfo(cData.schedule.end).militia) + ':00');
						obj.triggerChanges = false;
						chart.xAxis[0].setExtremes(min, max);

						//
						obj.printTimeSelectors();

						pos++;
						obj.timerPlayGraph = setTimeout(function () {
							play(pos);
						}, obj.graphTimePlay);
					}

					obj.prepareTheme(function () {
						onGetData();
						obj.updatePlayControls();
					})

				}else{
					obj.timePlaying = false;
					obj.updatePlayControls();
					obj.printTimeSelectors();
				}
			}
			play(0); //inicia de la primer posición
			preloadThemes(); 
		}
	},
	// Grafica
	createGraph: function (data) {
		var obj = this;
		var height = 315;
		if (data.cats.length > 15) {
			height = (height) + ((data.cats.length - 15) * 15);
		}

		Highcharts.chart('eod_graph_container', {
			chart: {
				type: 'bar',
				height: height,
				width: 515
			},
			lang: {
				printChart: 'Imprimir gráfica',
				downloadPNG: 'Descargar imágen PNG',
				downloadJPEG: 'Descargar imágen JPEG',
				downloadPDF: 'Descargar como PDF',
				downloadSVG: 'Descargar como SVG',
				contextButtonTitle: 'Menú contextual'
			},
			title: {
				text: ''
			},
			subtitle: {
				text: $('#eod_var_title').html()
			},
			xAxis: {
				categories: data.cats,
				title: {
					text: null
				}
			},
			yAxis: {
				min: 0,
				title: {
					text: 'Viajes',
					align: 'middle'
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
			tooltip: {
				valueSuffix: ''
			},
			plotOptions: {
				bar: {
					dataLabels: {
						enabled: true,
						allowOverlap: true,
						style: {
							fontSize: '9px'
						}
					}
				}
			},
			legend: {
				enabled: false,
				itemDistance: 20,
				layout: "vertical",
				verticalAlign: 'top',
				y: 55
			},
			credits: {
				enabled: false
			},
			series: data.series,
			exporting: {
				enabled: false
			}
		});
	}
});