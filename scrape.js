// Scrape revelvant data from https://insideairbnb.com/new-york-city/
// Paste this into the dev console
var options = $('#geoFilters')[0].options
var values = []
for (var i = 0; i < options.length; i++) {
  var option = options[i]
  values.push(option.value)
}

data = []

var scrape = (v) => {
  var valueClean = v || 'New York City'
  if (valueClean.indexOf('neighbourhood_group') >= 0) {
    valueClean = valueClean.split('|')[1]
  }

  var numListings = $('#summaryCount').text()
  var entireHomePercent = $('#summaryEntireHomePercentage').text()
  var pricePerNight = $('#summaryPrice').text()
  var privateRoomPercent = $('#summaryPrivateRoomMetricsPercentage').text()
  var sharedRoomPercent = $('#summarySharedRoomMetricsPercentage').text()
  var nightsPerYear = $('#summaryEstimatedNightsPerYear').text()
  var occupancy = $('#summaryEstimatedOccupancy').text()
  var monthlyIncome = $('#summaryEstimatedIncomePerMonth').text()
  var numHighAvailability = $('#summaryHighAvailabilityListings').text()
  var avgAvailability365 = $('#summaryAvailabilityDays').text()

  return [
    valueClean,
    numListings,
    entireHomePercent,
    pricePerNight,
    privateRoomPercent,
    sharedRoomPercent,
    nightsPerYear,
    occupancy,
    monthlyIncome,
    numHighAvailability,
    avgAvailability365,
  ]
}

values.forEach((value) => {
  loadGeo(value)
  var newData = scrape(value)
  console.log(newData)
  data.push(newData)
})

var csvContent =
  [
    'value',
    'numListings',
    'entireHomePercent',
    'pricePerNight',
    'privateRoomPercent',
    'sharedRoomPercent',
    'nightsPerYear',
    'occupancy',
    'monthlyIncome',
    'numHighAvailability',
    'avgAvailability365',
  ].join(',') + '\r\n'

data.forEach((row) => {
  const rowStr = row.join('\n').replace(/,/g, '').split('\n').join(',')
  csvContent += rowStr + '\r\n'
})
