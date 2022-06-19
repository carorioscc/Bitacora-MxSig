define([],function() {
    var tutorialDenue = {
	id:'tutorialDenue',
	root:'body',
	active:false,
	totalIcons:9,
	path:'tutorial/css/',
	clockTimer:null,
	clockIcon:null,
	actualIcon:1,
	defineTimer:function(){
	    var obj = this;
	    setTimeout(function(){
		obj.hide();
	    },6000);
	},
	create:function(){
	    var chain = this.getStructure();
	    $(this.root).append(chain);
	    this.active = true;
	    this.events();
	},
	getStructure:function(){
        var rows = [{type:"update",label:"Nuevos productos"},{type:"update",label:"5'081,192 establecimientos"},{type:"new",label:"Sección catastro"}];
        var itemRows = '';
        var left=63;
        for(var x in rows){
            
            itemRows+='<div style="position:absolute;left:'+left+'px;bottom:0px;"><img class="animationUpDown" src="tutorial/img/row_'+rows[x].type+'.png"><div class="labelRowTutorial" style="position:absolute;left:0px;bottom:155px;">'+rows[x].label+'</div></div>';
            left+=167;
        }
	    var chain=''+
	    '<div id="'+this.id+'">'+
		'<div class="labels">'+
		    //'<div class="description">Encuentra hoteles, restaurantes...</div>'+
		    '<div class="reference">¡CONSULTA LO NUEVO!</div>'+
		'</div>'+
        itemRows+
		//'<div class="marker"><div class="template_tutorialDenue ttd_marker"></div></div>'+
		//'<div class="icon_marker"><div id="icons_tuturial_denue" class="template_tutorialDenue ttd_icon1"></div></div>'+
		
	    '</div>';
	    
	    return chain;
	},
	hide:function(){
	    
	    if(this.active){
		clearInterval(this.clockIcon);
		$("#"+this.id).remove();
		this.active = false;
		
	    }
	    
	},
	events:function(){
	    var obj=this;
	    var item = $("#icons_tuturial_denue");
	    //this.clockIcon = setInterval(function(){
				    
				    obj.actualIcon+=1;
				    if(obj.actualIcon<=obj.totalIcons){
					var clase="ttd_icon";
					var prev = clase+ (obj.actualIcon-1);
					var next = clase+(obj.actualIcon);
					if(obj.actualIcon>0){
					    item.removeClass(prev);
					}
					
					item.addClass(next);
				    }else{
                        obj.hide();
				    }
				    
			    //},1000);
                setTimeout(function(){obj.hide()},8000);
	    
	    $(".layerManager_h_c_item_label").each(function(){
		$(this).click(function(){
		    obj.hide();
		});
		
	    });
	    $("#mdm6DinamicPanel").click(function(){
		obj.hide();
	    });
	    amplify.subscribe( "hideTutorialDenue", function() {
		//obj.hide();
	    });
         amplify.subscribe( "hideTutorial", function() {
            obj.hide();
	    });
        
        
	},
	loadCss:function(){
	    var obj = this;
	     $.when(
                    $('<link>', {rel: 'stylesheet',type: 'text/css',href: this.path+'tutorialDenue.css'}).appendTo('head'),
                    $.Deferred(function( deferred ){
                        $( deferred.resolve );
                    })
                ).done(function(){
			obj.create();
                });  
	},
	show:function(){
	    this.loadCss();
	}
    };
    
    return tutorialDenue;
});