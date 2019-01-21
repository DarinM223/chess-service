CREATE TABLE IF NOT EXISTS users (
    id INTEGER NOT NULL,
    name VARCHAR NOT NULL,
    password VARCHAR NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS games (
    id INTEGER NOT NULL,
    created_id INTEGER NOT NULL,
    other_id INTEGER NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (created_id) REFERENCES users (id),
    FOREIGN KEY (other_id) REFERENCES users (id)
);

CREATE TABLE IF NOT EXISTS moves (
    id INTEGER NOT NULL,
    game_id INTEGER NOT NULL,
    notation VARCHAR NOT NULL,
    FOREIGN KEY (game_id) REFERENCES games (id)
);
