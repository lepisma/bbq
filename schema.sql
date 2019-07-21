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
