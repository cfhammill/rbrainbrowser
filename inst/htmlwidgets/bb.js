HTMLWidgets.widget({
    
  name: "bb",
  
  type: "output",
  
  factory: function(el, width, height) {
    BrainBrowser.config.set("worker_dir", "");
    window.viewer = BrainBrowser.SurfaceViewer.start(el.id, function(v){return v;});
    viewer.render();
    
    return {
      renderValue: function(x) {

	if(x.hasOwnProperty("color_map")){
	  viewer.loadColorMapFromString(x.color_map);
	}

	viewer.addEventListener("displaymodel", function(e){
	  if(x.hasOwnProperty("intens")){
	    viewer.loadIntensityDataFromLocal(x.intens, "intens");
	  }
	});
	

	viewer.addEventListener("loadintensitydata", function(e){
	  console.log(e);
	  var min = viewer.model_data.get().intensity_data[0].min;
	  var max = viewer.model_data.get().intensity_data[0].max;
	  if(x.hasOwnProperty("min")) min = x.min;
	  if(x.hasOwnProperty("max")) max = x.max;
	  
	  viewer.setIntensityRange(min, max);
	});

	viewer.loadModelFromLocal(x.data, "brain.obj");
	
      },
      resize: function(width, height){
	viewer.updateViewport();
      },
      v: viewer
    };
  }
});
