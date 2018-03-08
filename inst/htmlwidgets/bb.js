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
	   
	viewer.loadModelFromLocal(x.data, "brain.obj");

	if(x.hasOwnProperty("intens")){
	  viewer.loadIntensityDataFromLocal(x.intens, "intens");
	}
      },
      resize: function(width, height){
	viewer.updateViewport();
      },
      v: viewer
    };
  }
});
