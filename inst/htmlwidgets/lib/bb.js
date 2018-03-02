console.log("starting up");
BrainBrowser.config.set("worker_dir", "/");
console.log("directory set");
BrainBrowser.SurfaceViewer.start("htmlwidget-1b9276c207f30ebae02a", function(bb){
    console.log("starting viewer");
    bb.render();
    bb.loadModelFromFile("hpf/largeprojects/MICe/chammill/2018-03-01_webgl-RMINC/inst/data/car.obj");
    $("#wireframe").change(function(e) {
	bb.setWireframe($(this).is(":checked"));
    });
});
