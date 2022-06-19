define(['dataSource','toolsConfig'],function(DataSource,toolsConfig){
	return{
		openDialog: function (url, title) {
			var obj = this;

			if (!$('#inegiheader-Info-dialog').attr('id')) {
				var cadena = '<div title="' + title + '" id="inegiheader-Info-dialog"><iframe id="inegiheader_html_doc" style="height:550px"></iframe></div>';
				$('#panel-center').append(cadena);
				$('#inegiheader-Info-dialog').dialog({
					dialogClass: 'inegiheader-Info-dialogContainer',
					width: 745,
					height: 595,
					modal: false,
					close: function (event, ui) {
						$(this).dialog('destroy').remove();
					}
				});
			} else {
				$('#inegiheader-Info-dialog').html('<div id="inegiheader-html-doc"></div>').dialog();
			}
			$('#inegiheader_html_doc').prop('src',url);
		},
		init:function(map,config){
			var obj = this;
			var lapseAttention = 2;
			var timerAttention = null;
			
			var cadena = '';
				<!--Ruta de navegación -->
				cadena+= '<div id="Encabezado" class="no-print" data-temselec="2" data-tema="1">';
  				
				cadena+= '</div>';
				
				var cadena2 = '<div id="custom_header_inegi_content" class="custom-header-inegi-content">';
					cadena2+= '	<img src="img/mdm_logo_inegi_header.png" height="38">';
					//cadena2+= '	<img id="header-mdm-eval" src="img/banner_eval.jpg" height="38">';
					cadena2+= '</div>';
					
					cadena2+= '<div class="custom-header-inegi-content-print" style="display:none">';
					cadena2+= '	<img src="img/headerINEGI.png" height="38" style="float:left">';
					cadena2+= '	<img src="img/mdm_logo_inegi_header.png" height="38" style="float:right">';
					cadena2+= '</div>';
					
				$('#header').html(cadena2).addClass('custom-header-inegi-site').attr('custom','inegi_site').show();
				$('body').prepend(cadena);	
			
				$('#header-mdm-eval').click(function(){
					obj.openDialog('http://igc.inegi.org.mx/igc_eval/faces/filterInit.jsp?def=mdm2018.q.xml&origen='+DataSource.mapEvaluation,'Opinión sobre el Mapa Digital de México en línea');
					$.cookie("mdm_evaluation_done", "true");
					clearTimeout(timerAttention);
				});
				
				
				var getAttention = function(){
					var cadena = '<div id="header_mdm_attention" class="header-mdm-attention">';
						cadena+= '	<div id="header_mdm_attention_text" class="header-mdm-attention-text" style="display:none">¡Ayúdanos a mejorar!<br><small>Tu opinión cuenta</small></div>';
						cadena+= '</div>';
					
					$('#header').prepend(cadena);
					$('#custom_header_inegi_content').addClass('floatEval');
					$('#header_mdm_attention').animate({width:400,height:400,top:-150,right:-150},1000,function(){
						$('#header_mdm_attention_text').fadeIn('slow');
						setTimeout(function(){
							$('#header_mdm_attention').animate({width:0,height:0,top:-150,right:-150},1500,function(){
								$('#header_mdm_attention_text').fadeOut('fast');
								$('#custom_header_inegi_content').removeClass('floatEval');
								$('#header_mdm_attention').remove();
							})
						},3500)
					});
				};
				var askEvaluation = function(){
					timerAttention = setTimeout(function(){
						if(!$.cookie("mdm_evaluation_done")){
							getAttention();
							askEvaluation();
						}
					},lapseAttention*1000*60);	
					lapseAttention+=2;  //preguntara a los 2 minutos, e ira incrementando el tiempo de pregunta en 2 minutos cada vez
				}
				//askEvaluation();
				
				
				
			
				$.when(
					$('<link>', {rel: 'stylesheet',type: 'text/css',href:'js/core/ui/inegiHeader.css'}).appendTo('head'),
					$.Deferred(function( deferred ){ 
						$( deferred.resolve );
					})
				).done(function(){
				
						$('#main').animate({top:'60px'},200,function(){
							$('#content').animate({top:'55px'},200,function(){
								//MDM6('updateSize'); //actualiza despliegue en mapa
							});
						});
					
					//si esta en producción carga header oficial
					if($(location).attr('hostname') == 'gaia.inegi.org.mx'){
						var hdInegi = toolsConfig.inegiHeaderPath;
						$.getScript(hdInegi);	
					}else{
					//de lo contrario carga un header local	
						var a = DataSource;
						
						$.when(
							$('<link>', {rel: 'stylesheet',type: 'text/css',href:'js/core/ui/inegiHeader_local.css'}).appendTo('head'),
							$.getScript("js/core/ui/inegiHeader_local.js"),
							$.Deferred(function( deferred ){ 
								$( deferred.resolve );
							})
						).done(function(){
							

						});
						
					}
				});
				
		}
	}
})

