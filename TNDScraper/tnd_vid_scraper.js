function myFunction() {

  //gets the Spreadsheet there
  var spreadSheet = SpreadsheetApp.getActiveSpreadsheet();
  var activeSheet = spreadSheet.getActiveSheet();
  activeSheet.clear()

  var delimiter = "\n";
  var myMaxResults = 50;
  var ratingList = [];
  var artistList = [];
  var albumTitleList = [];
  var genre_s = [];
  viewCountList = [];
  likeCountList = [];
  //uses the YouTube API to search for the tnd channel
  var search = YouTube.Search.list("snippet", {
    q: "REVIEW",
    //tnd channelID
    channelId: "UCt7fwAhXDy3oNFTAzF2o8Pw",
    //how many videos to scrape
    maxResults: 50,
    //in order of most recent date
    order: "date",
    publishedBefore: "2014-05-13T00:00:00Z"
    //                    month-day
  });

  //creates an array of the video data
  var videoData = [];


  var video = search.items[0];
  for (var i = 0; i < myMaxResults; i++){
    var video = search.items[i];
    var videoId = video.id.videoId;
    var videoTitle = video.snippet.title;

    var videoPublishedAt = video.snippet.publishedAt;

    var videoDetails = YouTube.Videos.list("snippet,contentDetails", {
      id: videoId,
    });

    var videoDescription = videoDetails.items[0].snippet.description;
    var videoDesc2 = videoDescription.split(delimiter).filter(text => text);
    ratingList.push(videoDesc2[videoDesc2.length - 4][0]);

    my_genre = videoDesc2[videoDesc2.length - 3].split(" / ")[3];
    var str1 = String(videoTitle).replace(" ALBUM REVIEW", "").split(" - ");
    var artists = str1[0];
    var albums = str1[1];
    var index = videoDescription.indexOf("/10");
    var rating = videoDescription.charAt(index - 1);

    videoData.push([videoId, videoTitle, artists, albums, rating, my_genre]);
  }

  if (videoData.length > 0){
    activeSheet.getRange(1, 1, videoData.length, videoData[0].length).setValues(videoData);
  }
}
