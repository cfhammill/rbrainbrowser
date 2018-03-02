HTMLWidgets.widget({
    
    name: "brainbrowser",
  
    type: "output",
  
    factory: function(el) {
	return {
	    renderValue: function(x) {
		console.log("starting up");
		BrainBrowser.config.set("worker_dir", x.dir)
		console.log("directory set");
		BrainBrowser.SurfaceViewer.start(el.id, function(bb){
		    console.log("starting viewer");
		    bb.render();
		    console.log(x.f);
		    bb.loadModelFromFile("/hpf/largeprojects/MICe/chammill/2018-03-01_webgl-RMINC/inst/data/car.obj");
		    $("#wireframe").change(function(e) {
			bb.setWireframe($(this).is(":checked"));
		    });
		});
	    },
	    resize: function(){},
	};
    }
});
