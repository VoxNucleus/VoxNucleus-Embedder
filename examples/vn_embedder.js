/**
 * Description : This is the first version of the embedder, javascript side.
 * HOWTO use : This plugin uses Jquery (sorry too painful to use vanilla javascript)
 * First set up Erlembedder to run as a server on THE SAME PORT AS THE PAGE FROM WHICH YOU ARE ACCESSING THE PAGE CONTAINING THE SCRIPT(see http://www.victorkabdebon.com for some details on how to do it for simple cases). The address can either be modified here in the source or just by calling $.vn_embedder({embedder_server:'http://yourserver.wat/ErlEmbedderAddress'})
 * This version performs two operations: First one is verification. For all the links on which this thing is applied a first call is made to verify that it can be "embed". For the one that are successful a button/ text will be inserted next to the link to propose to embed.
 * The target class must be in the SAME container as vn_emb and must fall in the target_class. This script will specifically go and search for the "href" param in the tag.
 *
 * Website : http://www.voxnucleus.fr
 * Author : Victor Kabdebon ( http://www.victorkabdebon.com )
 * License : GPLv2, Please respect this license, thank you very much.
 * Version : v0.2
 * (Compatible with ErlEmbedder v0.2, not earlier)
 **/

(function($){
   $.fn.vn_embedder= function(params){
     var default_params={
       verification:true,
       container_class:'div',
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

       var vid_address=$(this).attr("href");
       var to_embed=this;
       var verif_opt={
	 url:params.embedder_server+"/verification",
	 type:"GET",
	 data:{
	   requesting:'ispresent',
	   from:vid_address},
       success:function(data, textStatus, jqXHR){

	 if(data.success=="true"){
	   if(data.present=="true"){
	     //
	     var old_code=$(to_embed).clone();

	     var new_dom=document.createElement("div");
	     new_dom.setAttribute("class","embedder_container");
	     var new_dom_trig=document.createElement("div");
	     new_dom_trig.setAttribute("class","trigger");
	     new_dom_trig.innerHTML="Embed";
	     new_dom.appendChild(new_dom_trig);
	     $(new_dom).append(old_code);
	     $(to_embed).replaceWith(new_dom);
	     //on click
	     $(new_dom_trig).click(function(){
	       var options={
		 url:params.embedder_server+"/request",
		 type:"GET",
		 data:{
		   from:vid_address,
		   width:params.width,
		   height:params.height,
		   hd:params.hd},
		 success:function(data, textStatus, jqXHR){
		   if(data.status=="found"){
		     //Set the trigger to expended
		     //$(trigger).attr("expended","expended");
		     $(new_dom).append(
		     data.code
		   );
		   }else if(data.status=="error"){
		     $(parent).append("<span class=\"emb_error\">Erreur</span>"+data.explanation);
		   }else if(data.status=="notfound"){
		     $(parent).append("<span class=\"emb_notfound\">Non trouve</span>");
		   }
		 },
		 error:function(jqXHR, textStatus, errorThrown){
		   $(parent).append("<span class=\"emb_error\">Erreur du serveur</span>");
		 },
		 dataType:"json"
	       };
	     $.ajax(options);

	       });

	   }else{
	     //Impossible to embed something
	   }
	 }else{
	   //does not correspond to anything
	 }

       },
       error:function(jqXHR, textStatus, errorThrown){
	 //TODO : Problem occured here
       }
       };
       $.ajax(verif_opt);
/*
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
	       });*/


       });

   };
 })(jQuery);
