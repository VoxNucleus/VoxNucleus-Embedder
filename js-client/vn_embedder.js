/**
 * Description : This is the first version of the embedder, javascript side.
 * HOWTO use : This plugin uses Jquery (sorry too painful to use vanilla javascript)
 * First set up the embedder_server, it can either be modified here in the source or just by calling $.vn_embedder({embedder_server:'Your server address'})
 * In that version, and it's going to change, you need a two level organization. A container that holds 2 classes, the first one is vn_emb which when clicked will send a request to the embedder.
 * The target class must be in the SAME container as vn_emb and must fall in the target_class. This script will specifically go and search for the "href" param in the tag.
 * Website : http://www.voxnucleus.fr
 * Author : Victor Kabdebon ( http://www.victorkabdebon.net )
 * License : GPLv2, Please respect this license, thank you very much.
 * Version : v0.1
 **/

(function($){
   $.fn.vn_embedder= function(params){
     var default_params={
       trigger_class:'vn_emb',
       target_class:'vn_emb_target',
       embedder_target:"",
       vid_params:{
	 hd:true,
	 height:500,
	 width:500
       },
       embedder_server:'/embedder-server',
       jsonp:false
     };
     var parent=document.body;
     var trigger=null;
     params = $.extend(default_params,params);

     this.each(function(){
     trigger=this;
     parent=$(this).parent();
     parent.find("."+params.trigger_class).click(function(){
       var vid_address=$(parent).find("."+params.target_class).attr("href");
       var options={
	 url:params.embedder_server,
	 type:"GET",
	 data:{from:vid_address,
	       width:params.width,
	       height:params.height,
	       hd:params.hd},
	 success:function(data, textStatus, jqXHR){
	   if(data.status=="found"){
	     //Set the trigger to expended
	     $(trigger).attr("expended","expended");
	     $(parent).append(
	       data.code
	     );
	   }else if(data.status=="error"){
	     $(parent).append("<span class=\"emb_error\">Erreur</span>");
	   }else if(data.status=="notfound"){
	     $(parent).append("<span class=\"emb_notfound\">Non trouve</span>");
	   }
	 },
	 error:function(jqXHR, textStatus, errorThrown){
	   $(parent).append("<span class=\"emb_error\">Une erreur server s\'est produite</span>");
	 },
	 dataType:"json"

       };
       $.ajax(options);
     });


       });

   };
 })(jQuery);
