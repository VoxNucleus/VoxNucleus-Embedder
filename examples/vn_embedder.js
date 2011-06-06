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
       container_class:'vn_vid_cont',
       trigger_class:'vn_emb',
       target_class:'vn_emb_target',
       embedder_target_id:"",
       vid_params:{
	 hd:true,
	 height:500,
	 width:500
       },
       messages:{
	 server_error:"Erreur serveur",
	 error:"Erreur",
	 not_found:"Non trouve",
	 trigger_code:"Lancer video"
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
			 var old_code=$(to_embed).clone();
			 var new_dom= new_dom=document.createElement("div");
			 new_dom.setAttribute("class","embedder_container");
			 var new_dom_trig=document.createElement("div");
			 new_dom_trig.setAttribute("class","trigger");
			 new_dom_trig.innerHTML=params.messages.trigger_code;
			 new_dom.appendChild(new_dom_trig);
			 $(new_dom).append(old_code);
			 $(to_embed).replaceWith(new_dom);


			 //Here we verify if there is a object with specified id where we can
			// if(embedder_target_id!="" &&
			//    $("#"+embedder_target_id).length>0){
			//   new_dom=$("#"+embedder_target_id);
			// }

			 //on click
			 $(new_dom_trig)
			   .click(function(){
				    // Here we verify if there is a object with specified id where we can
				    var where_to_insert=$(this).parent();
				    if(params.embedder_target_id!="" &&
				       $("#"+params.embedder_target_id).length>0){
				      where_to_insert=$("#"+params.embedder_target_id);
				    }

				    var is_expended=$(this).attr("vn_expanded");
				    if(is_expended=="true"){
				      //Minimize & delete

				      where_to_insert
					.find("."+params.container_class)
					.slideUp('slow',function(){
						   $(this).remove();
						   $(new_dom_trig).attr("vn_expanded","false");
						 });
				    }else{
				      //Query normally
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
					    var video_container = document.createElement("div");
					    $(video_container).addClass(params.container_class);
					    $(video_container).append(data.code);
					    $(where_to_insert).append(video_container);
					  }else if(data.status=="error"){
					    $(where_to_insert).append("<span class=\" "+params.container_class+" emb_error\">"+params.messages.error+"</span>"+data.explanation);
					  }else if(data.status=="notfound"){
					    $(where_to_insert).append("<span class=\" "+params.container_class+" emb_notfound\">"+params.messages.not_found+"</span>");
					  }
					  $(new_dom_trig).attr("vn_expanded","true");
					},
					error:function(jqXHR, textStatus, errorThrown){
					  $(where_to_insert).append("<span class=\""+params.target_class+" emb_error\">"+params.messages.server_error+"</span>");
					},
					dataType:"json"
				      };
				      $.ajax(options);
				    }

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
	       });

   };
 })(jQuery);
