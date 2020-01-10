-- Songs table contains basic information and id
CREATE TABLE songs (
	id INTEGER NOT NULL, title TEXT, url TEXT, artist TEXT, album TEXT, mtime INTEGER,
	PRIMARY KEY (id)
);

-- Sources are used for resolving song urls
CREATE TABLE sources (
	id INTEGER NOT NULL, resolver TEXT, name TEXT, url TEXT, inc BOOLEAN,
	PRIMARY KEY (id)
);

-- Log of songs played
CREATE TABLE play_log (
  id INTEGER PRIMARY KEY,
  time INTEGER NOT NULL,
  song_id INTEGER NOT NULL,
  FOREIGN KEY(song_id) REFERENCES songs(id)
);

-- List of tags from Last.fm. We are assuming that tags have same namespace.
CREATE TABLE lastfm_tags (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

-- Log from Last.fm saved using dump https://benjaminbenben.com/lastfm-to-csv/
-- This is not updated and only used for certain analyses
CREATE TABLE lastfm_log (
  id INTEGER PRIMARY KEY,
  artist TEXT NOT NULL,
  album TEXT,
  title TEXT NOT NULL,
  timestring TEXT NOT NULL
);
