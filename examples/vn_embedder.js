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
 * Version : v0.4.1 (follows Erlembedder notation for 0.x and then real client version)
 * Compatibility : v0.4 for ErlEmbedder Jquery 1.4.2+
 **/


/**
 * Function that parses param of the form
 * [param1:value1;...]
 * Input : A string or null value(it will )
 * Returns : An object of the form {param1:value1,param2:value2} (might be empty {})
 */
function parse_vnparam(default_options,input){
  var result={};
  $.extend(true,result,default_options);
  try{
    if(input){
      var split_res=input.split(";");
      for(var index=0;index<split_res.length;index++){
	var fragment=split_res[index];
	var split_fragment_res=fragment.split(":");
	//Only do something if there are at least 2 values in it
	if(split_fragment_res.length>1){
	  result[split_fragment_res[0]]=split_fragment_res[1];
	}
      }
    }
  }catch(err){
    result=default_options;
  }finally{
    return result;
  }
}

/**
 * Parse a shortcode string of the form website.ext:key;param1:value1
 * Returns an object of the form {website:website.ext,key:video-key,param1:value1,param2,value2}
 */
function parse_shortcode(shortcode_string){
  var result={};
  try{
    if(shortcode_string){
      var split_res=shortcode_string.split(";");
      // Parse website and key
      var first_part=split_res[0].split(":");
      result['website']=first_part[0];
      result['key']=first_part[1];
      //Parse the rest of the options
      for(var index=1;index< split_res.length;index++){
	var fragment=split_res[index];
	var split_fragment_res=fragment.split(":");
	//Only do something if there are at least 2 values in it
	if(split_fragment_res.length>1){
	  result[split_fragment_res[0]]=split_fragment_res[1];
	}
      }
    }
  }catch(err){
    result={};
  }finally{
    return result;
  }
}
/**
 * Build the verification request
 * Output : Javascript object
 */
function b_verif_req(href,shortcode){
  var data_arg={
    requesting:'ispresent'
  };
  if(shortcode){
    data_arg['shortcode']=true;
    var parsed_infos=parse_shortcode(shortcode);
    data_arg=$.extend(true,data_arg,parsed_infos);
  } else if(href){
    data_arg['from']=href;
  }
  return data_arg;
}
/**
 * Build the embedding request
 * Output: Javascript object
 */
function b_embed_req(href,shortcode,options){
  var vid_infos={};
  //Merge default options with vid_infos
  vid_infos=$.extend(true,vid_infos,options);
  if(shortcode){
    //set short to true
    vid_infos['shortcode']=true;
    var parsed_infos=parse_shortcode(shortcode);
    //Merge shortcode
    vid_infos=$.extend(true,vid_infos,parsed_infos);
  } else if(href){
    vid_infos['from']=href;
  }
  return vid_infos;
}

/**
 * Plugin code.
 ** Parameters explanation:
 * verification: true|false -> Verify first : Useless at the moment
 * auto_expand: true|false -> useless at the moment
 * container_class : (string) -> Class of the container (class ?)
 * target_class : (string) ->
 * vid_params : (js_object) -> Parameters of the videos, depends on your configuration
 * tclass : (string)-> Class of the triggger. Important if one wants to
 * class_expanded: (string) -> Name of the class once expanded
 * internal_code (string/DOM)-> Code inside the trigger
 * title (string) -> Title when mouse over trigger
 * messages (js_object)-> Set of messages used by the program to indicate errors
 * embedder_server (string) -> Address of the ErlEmbedder server
 * jsonp (boolean) -> useless at the moment
 */

(function($){
   $.fn.vn_embedder= function(params){
     var default_params={
       verification:true,
       auto_expand:false,
       container_class:'vn_vid_cont',
       target_class:'vn_emb_target',
       embedder_target_id:"",
       shortcode_param:'shortcode',
       vid_params:{
	 hd:true
       },
       trigger:{
	 tclass:'trigger',
	 class_expanded:'vn_emb_exp',
	 internal_code:"",
	 title:"Play"
       },
       messages:{
	 server_error:"Erreur serveur",
	 error:"Erreur",
	 not_found:"Non trouve"
       },
       actions:{
	 verif_error:function(){
	   alert("Verification error");
	 },
	 embedding_error:function(){
	   alert("Embedding error");
	 },
	 notfound_error:function(){
	   alert("Not found");
	 }
       },
       embedder_server:'/embedder-server',
       jsonp:false
     };
     params = $.extend(true,params,default_params);
     this.each(
       function(){
	 var vid_address=$(this).attr("href");
	 var li_shortcode=$(this).attr(params.shortcode_param);
	 var to_embed=this;
	 var verif_opt={
	   url:params.embedder_server+"/verification",
	   type:"GET",
	   data:b_verif_req(vid_address,li_shortcode),
	   success:function(data, textStatus, jqXHR){
	     if(data.success=="true"){
	       if(data.present=="true"){
		 parse_vnparam(params.vid_params,$(to_embed).attr("vn_emb_param"));
		 var old_code=$(to_embed).clone();
		 var new_dom= new_dom=document.createElement("div");
		 new_dom.setAttribute("class","embedder_container");
		 var new_dom_trig=document.createElement("span");
		 new_dom_trig.setAttribute("class",params.trigger.tclass);
		 new_dom_trig.setAttribute("title",params.trigger.title),
		 $(new_dom_trig).data("emb_param",parse_vnparam($(to_embed).attr("vn_emb_param")));
		 $(new_dom_trig).data("associated_address",vid_address);
		 new_dom_trig.innerHTML=params.trigger.internal_code;
		 new_dom.appendChild(new_dom_trig);
		 $(new_dom).append(old_code);
		 $(to_embed).replaceWith(new_dom);
		 //on click
		 $(new_dom_trig)
		   .click(function(){
			    //Reinitialize
			    var address = $(this).data("associated_address");
			    // Here we verify if there is a object with specified id where we can
			    var where_to_insert=$(this).parent();
			    if(params.embedder_target_id!="" &&
			       $("#"+params.embedder_target_id).length>0){
			      where_to_insert=$("#"+params.embedder_target_id);
			    }
			    var inline_params=$(this).data("emb_param");
			    var is_expended=$(this).attr("vn_expanded");
			    if(is_expended=="true"){
			      //Minimize & delete
			      $(this).removeClass(params.trigger.class_expanded);
			      where_to_insert
				.find("."+params.container_class)
				.slideUp('slow',function(){
					   $(this).remove();
					   $(new_dom_trig).attr("vn_expanded","false");
					 });
			    }else{
			      $(this).addClass(params.trigger.class_expanded);
			      var data_options=b_embed_req(address,li_shortcode,params.vid_params);
			      //Merge inline parameters into data_options
			      data_options=$.extend(true,data_options,inline_params);
			      //Query normally
			      var options={
				url:params.embedder_server+"/request",
				type:"GET",
				data:data_options,
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
		 params.actions.embedding_error();
	       }
	     }else{
	       params.actions.notfound_error();
	     }

	   },
	   error:function(jqXHR, textStatus, errorThrown){
	     params.actions.verif_error();
	   }
	 };
	 $.ajax(verif_opt);
       });

   };
 })(jQuery);
