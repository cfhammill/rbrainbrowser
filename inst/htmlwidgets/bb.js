HTMLWidgets.widget({
    
  name: "bb",
  
  type: "output",
  
  factory: function(el, width, height) {
    BrainBrowser.config.set("worker_dir", "");
    var viewer = BrainBrowser.SurfaceViewer.start(el, function(v){return v;});
    var THREE = BrainBrowser.SurfaceViewer.THREE
    el.viewer = viewer;
    viewer.render();
    viewer.setClearColor("0xFFFFFF", 0);
    
    return {
      renderValue: function(x) {
        viewer.clearScreen();

        el.style.backgroundColor = x.bg_colour;
        
        if(x.bg_plot !== undefined){
          el.style.backgroundImage = x.bg_plot;
        }
        
        if(x.hasOwnProperty("debug") && x.debug){
          window.viewer = viewer;
        }
        
	if(x.hasOwnProperty("color_map")){
	  viewer.loadColorMapFromString(x.color_map);
	}

	viewer.addEventListener("displaymodel", function(e){
          if(x.hasOwnProperty("zoom")){
            viewer.zoom = x.zoom;
          }

          if(x.hasOwnProperty("rotation")){
            viewer.model.rotation.x = x.rotation.x;
            viewer.model.rotation.y = x.rotation.y;
            viewer.model.rotation.z = x.rotation.z;
          }

          // if(x.hasOwnProperty("matrix")){
          //   viewer.model.matrixAutoUpdate = false;
          //   var m = new THREE.Matrix4();
          //   m.set(x.matrix)
          //   viewer.model.matrix = m;
          // }

	  if(x.hasOwnProperty("intens")){
	    viewer.loadIntensityDataFromLocal(x.intens, "intens");
	  }
	}, {once : true});
	

	viewer.addEventListener("loadintensitydata", function(e){
	  console.log(e);
	  var min = viewer.model_data.get().intensity_data[0].min;
	  var max = viewer.model_data.get().intensity_data[0].max;
	  if(x.hasOwnProperty("min")) min = x.min;
	  if(x.hasOwnProperty("max")) max = x.max;
	  
	  viewer.setIntensityRange(min, max);
	}, {once : true});

          
	viewer.loadModelFromLocal(x.data, "brain.obj");
      },
      resize: function(width, height){
	viewer.updateViewport();
      },
      v: viewer
    };
  }
});
