module.exports = ['$http', '$scope', 'Entries',
		  function($http, $scope, Entries) {
		      var bigRE = /_\d+\.([^.]+)$/
		      function bigUrl(e) {
			  return e && e.imageUrl.replace(bigRE, "_1280.$1");
		      }

		      function thumbUrl(e) {
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

		      $scope.selectedEntry = null;
		      $scope.entries = null;

		      $scope.selectEntry = function(e) {
			  $scope.selectedEntry = e;
		      };

		      $scope.thumbUrl = thumbUrl;
		      $scope.bigUrl = bigUrl;
		      $scope.saveEntry = saveEntry;
		      $scope.unsaveEntry = unsaveEntry;

		      Entries.query(function(data) {
			  $scope.entries = data;
			  if (data.length > 0) {
			      $scope.selectedEntry = data[0];
			  }
		      });
		  }]
