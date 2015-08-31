var Crawler = require('./crawler.js');


module.exports = ['$http', '$scope', 'Entries', 'toastr',
		  function($http, $scope, Entries, toastr) {
		      var bigRE = /_\d+\.([^.]+)$/
		      function bigUrl(e) {
// return "http://7-themes.com/data_images/out/69/7009683-innocent-puppy-eyes.jpg"
			  return e && e.imageUrl.replace(bigRE, "_1280.$1");
		      }

		      function thumbUrl(e) {
// return "https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcSkxdNK4qaUDQ_b8IoEgJs17XNtRPVgB3UdcNmo2y3Sjv1SKgMReNGHAQ"
			  return e && e.imageUrl.replace(bigRE, "_100.$1");
		      }

		      function saveEntry(e) {
			  $http({
			      method: "POST",
			      url: "/saveUrl",
			      transformRequest: function(obj) {
				  var str = [];
				  for(var p in obj)
				      str.push(encodeURIComponent(p) + "=" +
					       encodeURIComponent(obj[p]));
				  return str.join("&");
			      },
			      data: { url: bigUrl(e) },
			      headers: {
				  'Content-Type': 'application/x-www-form-urlencoded'
			      }
			  }).then(function() {
			      toastr.success('Saved');
			  }, function() {
			      toastr.error('Failed to save image.');
			  });
		      }

		      function unsaveEntry(e) {
			  $http({
			      method: "POST",
			      url: "/unsaveEntry",
			      transformRequest: function(obj) {
				  var str = [];
				  for(var p in obj)
				      str.push(encodeURIComponent(p) + "=" +
					       encodeURIComponent(obj[p]));
				  return str.join("&");
			      },
			      data: { entryId: e.entryId },
			      headers: {
				  'Content-Type': 'application/x-www-form-urlencoded'
			      }
			  }).then(function() {
			      toastr.success('Unstarred');
			  }, function() {
			      toastr.error('Failed to unstar entry.');
			  });
			  console.log("unsave");
		      }

		      function selectIndex(i) {
			  $scope.selectedIndex_ = i;
		      }

		      function selectedIndex() {
			  return $scope.selectedIndex_;
		      }

		      function selectedEntry() {
			  return $scope.entries[selectedIndex()];
		      }

		      function refresh() {
			  $scope.entries = [];
			  Entries.query(function(data) {
			      $scope.entries = data;
			      if (data.length <= $scope.selectedIndex_) {
				  $scope.selectIndex(data.length - 1);
			      }
			      toastr.success('Loaded ' + data.length + ' entries.');
			  });
		      }

		      function crawlEntry(e) {
			  if (e.crawler) return;

			  e.crawler = new Crawler($scope, e, bigUrl(e));
		      }

		      // Instance vars
		      $scope.selectedIndex_ = 0;
		      $scope.entries = null;

		      // Methods
		      $scope.selectIndex = selectIndex;
		      $scope.selectedIndex = selectedIndex;
		      $scope.selectedEntry = selectedEntry;
		      $scope.thumbUrl = thumbUrl;
		      $scope.bigUrl = bigUrl;
		      $scope.crawlEntry = crawlEntry;
		      $scope.saveEntry = saveEntry;
		      $scope.unsaveEntry = unsaveEntry;
		      $scope.refresh = refresh;

		      refresh();
		  }]
