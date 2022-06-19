// JavaScript Document
window.layerWidgets.ccpv2010 = {
	widgetLoaded:false,
	widgetActive:false,
	init:function(opc){
		var obj = this;
		obj.layer = opc.layer;
		obj.map = opc.map;
		obj.id = obj.layer.idLayer;
		//carga de archivos necesarios
		var path = 'config/layerWidgets/'+obj.layer.idLayer+'/'
		$.when(
			$.getScript(path+'config.js'),
			$('<link>', {rel: 'stylesheet',type: 'text/css',href:path+'widget/jquery.ui.'+obj.layer.idLayer+'.css'}).appendTo('head'),
			$.getScript( path+'widget/jquery.ui.'+obj.layer.idLayer+'.js'),
            $.Deferred(function( deferred ){
                $( deferred.resolve );
            })
        ).done(function(){
			if(obj.widgetLoaded){
				obj.initWidget();
			}
		});
	},
	remove:function(){
		var obj = this;
		var id = obj.id;
		obj.widgetActive = false;
		
		if($('#'+'mdmwidgetlayer'+id).attr('id')) //si ya esta abierto lo elimina y lo vuelve a crear
				$('#'+'mdmwidgetlayer'+id).remove();	
	},
	events:function(opc){ //eventos del MDM
		var obj = this;
		var id = obj.id;
		obj.event = opc;
		switch(obj.event.event){
			case 'identify':
				$('#'+'mdmwidgetlayer'+id).ccpv2010('onIdentify',obj.event.data);
			break;
			default: //de no caer el evento en ninguna de las opciones ejecuta el callback del evento que normalmente es para dar continuidad al evento como tal dentro del MDM
				obj.event.callback(obj.event.data);
			break;
				
		}
	},
	initWidget:function(){
			var obj = this;
			var map = obj.map;
			var id = obj.id;
			
			obj.widgetActive = true;
		
			var config = $.extend({},true,obj.config);
			obj.nameAcive = 'mdmwidgetlayer_'+id;
			
			if($('#'+'mdmwidgetlayer'+id).attr('id')) //si ya esta abierto lo elimina y lo vuelve a crear
				$('#'+'mdmwidgetlayer'+id).remove();
				
			var cadena = '<div id="mdmwidgetlayer'+id+'"></div>';
			$("#panel-center").append(cadena);
			
			$('#'+'mdmwidgetlayer'+id).ccpv2010({
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
								/*
								MDM6('setParams',{
									layer:'Economico',
									params:{forceRefresh:true}
									});
									*/
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

							var dialog = '<div id="cenagoNotification" title="'+((options && options.title)?options.title:'Metadatos')+'">';
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
//@ sourceURL=config/layerWidgets/cwidgetbase/control.js