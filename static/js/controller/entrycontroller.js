module.exports = ['$scope', 'Entries',
		  function($scope, Entries) {
		      var bigRE = /_\d+\.([^.]+)$/
		      function bigUrl(e) {
			  return e.imageUrl.replace(bigRE, "_1280.$1");
		      }

		      function thumbUrl(e) {
			  return e.imageUrl.replace(bigRE, "_100.$1");
		      }

		      $scope.selectedEntry = null;
		      $scope.entries = null;

		      $scope.selectEntry = function(e) {
			  $scope.selectedEntry = e;
		      };

		      $scope.thumbUrl = thumbUrl;
		      $scope.bigUrl = bigUrl;

		      Entries.query(function(data) {
			  $scope.entries = data;
			  if (data.length > 0) {
			      $scope.selectedEntry = data[0];
			  }
		      });
		  }]
