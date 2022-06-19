define(function(){
	return{
		currentActive:null,
		nameActive:null,
		config:null,
		init:function(map){
			this.map = map;
			
		},
		load:function(id){
			var obj = this;
			var GT_config = window['MDMXSIG_layer_'+id];  //revisa si ya fue cargada la variable del widget
			if(!GT_config){
				var path = 'config/genericthemes/'+id;
				$.when(
					$.getScript( path+'/'+id+'.js' ),
					$('<link>', {rel: 'stylesheet',type: 'text/css',href: path+'/'+id+'.css'}).appendTo('head'),
					$.Deferred(function( deferred ){
						$( deferred.resolve );
					})
				).done(function(){
					obj.config = $.extend({},window['MDMXSIG_layer_'+id]);
					obj.loadWidget(id);
				})
			}else{
				obj.config = $.extend({},window['MDMXSIG_layer_'+id]);
				obj.loadWidget(id);
			}
		},
		unload:function(id){
			var obj = this;
			$('#'+'mdmxsig_gt_'+id).remove();
			obj.currentActive = null;
			obj.nameActive = null;
			obj.config = null;
		},
		loadWidget:function(id){
			var obj = this;
			var map = obj.map;
			
			var config = obj.config;
			obj.nameAcive = 'mdmxsig_gt_'+id;
			
			if($('#'+'mdmxsig_gt_'+id).attr('id')) //si ya esta abierto lo elimina y lo vuelve a crear
				$('#'+'mdmxsig_gt_'+id).remove();
				
			var cadena = '<div id="mdmxsig_gt_'+id+'"></div>';
			$("#panel-center").append(cadena);
			
			$('#'+'mdmxsig_gt_'+id).generictheme({
				
						map:map,
						config:config,
						onActive:function(){
							/*
							geoelectoralesConfig.baseMapBeforeStart = $("#mdm6Layers").layerManager('getCurrentBaseMap');
							$("#mdm6Layers").layerManager('closeRightCarrucel');
							$('#mdm6Layers').addClass('hideBaseMap hidetreelayer');

							setTimeout(function(){
								$("#mdm6Layers_layerManager_collapsedTools").Popup({content:'Se deshabilitó la selección de mapa base y capas de forma temporal',showOn:'now',highlight :true,time:10000});
							},1000);
							*/
						},
						onClose:function(){
							/*
							$('#mdm6Layers').removeClass('hideBaseMap hidetreelayer');
							$("#mdm6Layers").layerManager('setBaseMap',geoelectoralesConfig.baseMapBeforeStart);
							*/
						},
						getResolution:function(){
							return map.getResolution();
						},
						refreshMap:function(params){
							if(params){
								MDM6('setParams',{
									layer:'Economico',
									params:params
									});
							}else{
								MDM6('setParams',{
									layer:'Economico',
									params:{forceRefresh:true}
									});
							}
						},
						onTransparency:function(data){
							MDM6('setOpacity','Economico',data/100);
						},
						onStart:function(){
							$("#mdm6Layers").layerManager('closeBottomCarrucel');
						},
						extent:function(wkt){
							map.goCoords(wkt);	
						},
						onIdentifyFail:function(data){
							$("#mdm6DinamicPanel").dinamicPanel('identifyPoint',data);  
						},
						onCloseIdentify:function(){
							MDM6('hideMarkers','identify');	
						},
					   detectCollision:function(element){
							var dp_top = $('#mdm6DinamicPanel').position().top+90;
							var a_top = element.position().top;
							if(dp_top > a_top){
								$("#mdm6DinamicPanel").dinamicPanel('hideSearch');
							}
						},
						systemMessage:function(msg,options){
							if(!options)
								map.Notification({message:msg,time:3000});

							var dialog = '<div id="cenagoNotification" title="'+((options && options.title)?options.title:'Información')+'">';
								dialog+= msg;
								dialog+= '</div>';

							$('body').append(dialog);

							var dialogDefault = {
								resizable:false,
								width:350,
								modal: true,
								height:250,
								close: function(event, ui){
									$(this).dialog('destroy').remove();
								},
								buttons: {
									"Aceptar": function() {
									  $( this ).dialog( "close" );
									}
								  }
								}

							for(var x in options)
								dialogDefault[x] = options[x];	

							$('#cenagoNotification').dialog(dialogDefault);
						}
			});
			
			obj.currentActive = $('#'+'mdmxsig_gt_'+id);
			
		}
	}
});
