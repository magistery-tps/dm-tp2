db.getCollection('lyrics-english').aggregate([
    {
        $addFields: { 
          track_artist: { $concat: [ "$track_name", "$artist_name" ] } 
        }
    },
    { $out: "lyrics_english_with_id" }
]);

db.getCollection('lyrics-spanish').aggregate([
    {
        $addFields: { 
          track_artist: { $concat: [ "$track_name", "$artist_name" ] } 
        }
    },
    { $out: "lyrics_spanish_with_id" }
]);

db.getCollection('lyrics_english_with_id').aggregate([
    { 
        $project: { 
            track_name: 1, 
            artist_name: 1, 
            track_artist: 1,
            lyric: "$lyrics" 
        }
    },
    { 
        $unionWith: { 
            coll: "lyrics_spanish_with_id", 
            pipeline: [ 
                { 
                    $project: { 
                        track_name: 1, 
                        artist_name: 1, 
                        track_artist: 1,
                        lyric: "$lyrics"
                    }
                }
            ]
        }
    },
    { $out: "lyrics" }
 ]);

db.track_features_top_200.aggregate([
    {
        $lookup: {
            from: "lyrics",
            foreignField: "track_artist",
            localField: "track_artist", 
            as: "result"
        }
    },
    {
        $match: { result: { $exists: true, $not: {$size: 0} } }
    },
    {
        $addFields: { 
            lyric: { "$arrayElemAt": ["$result.lyric", 0] }
        }
    },
    { 
        $project: { 
            _id: 0,
            result: 0, 
            markets: 0 
        } 
    },
    { $out: "track_features_top_200_lyric" }
]);