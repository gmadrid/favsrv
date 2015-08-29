module.exports = ['$http', '$scope', 'Entries',
		  function($http, $scope, Entries) {
		      var bigRE = /_\d+\.([^.]+)$/
		      function bigUrl(e) {
//			  return "http://7-themes.com/data_images/out/69/7009683-innocent-puppy-eyes.jpg"
			  return e && e.imageUrl.replace(bigRE, "_1280.$1");
		      }

		      function thumbUrl(e) {
//			  return "https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcSkxdNK4qaUDQ_b8IoEgJs17XNtRPVgB3UdcNmo2y3Sjv1SKgMReNGHAQ"
			  return e && e.imageUrl.replace(bigRE, "_100.$1");
		      }

		      function saveEntry(e) {
			  $http({
			      method: "POST",
			      url: "/saveUrl",
			      transformRequest: function(obj) {
				  var str = [];
				  for(var p in obj)
				      str.push(encodeURIComponent(p) + "=" + encodeURIComponent(obj[p]));
				  return str.join("&");
			      },
			      data: { url: bigUrl(e) },
			      headers: {
				  'Content-Type': 'application/x-www-form-urlencoded'
			      }
			  });
			  console.log("save");
		      }

		      function unsaveEntry(e) {
			  $http({
			      method: "POST",
			      url: "/unsaveEntry",
			      transformRequest: function(obj) {
				  var str = [];
				  for(var p in obj)
				      str.push(encodeURIComponent(p) + "=" + encodeURIComponent(obj[p]));
				  return str.join("&");
			      },
			      data: { entryId: e.entryId },
			      headers: {
				  'Content-Type': 'application/x-www-form-urlencoded'
			      }
			  });
			  console.log("unsave");
		      }

		      function selectEntry(e) {
			  $scope.selectedEntry = e;
		      }

		      function refresh() {
			  $scope.entries = [];
			  Entries.query(function(data) {
			      $scope.entries = data;
			      if (data.length > 0) {
				  $scope.selectedEntry = data[0];
			      }
			  });
		      }

		      // Instance vars
		      $scope.selectedEntry = null;
		      $scope.entries = null;

		      // Methods
		      $scope.selectEntry = selectEntry;
		      $scope.thumbUrl = thumbUrl;
		      $scope.bigUrl = bigUrl;
		      $scope.saveEntry = saveEntry;
		      $scope.unsaveEntry = unsaveEntry;
		      $scope.refresh = refresh;
		      
		      refresh();
		  }]
