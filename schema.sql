-- Songs table contains basic information and id
CREATE TABLE songs (
	id INTEGER NOT NULL, title TEXT, url TEXT, artist TEXT, album TEXT, mtime INTEGER,
	PRIMARY KEY (id)
);

-- Sources are used for resolving song urls
CREATE TABLE sources (
	id INTEGER NOT NULL, resolver TEXT, name TEXT, url TEXT, inc BOOLEAN,
	PRIMARY KEY (id)
)
