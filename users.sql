CREATE TABLE IF NOT EXISTS users (
    id SERIAL,
    name VARCHAR NOT NULL,
    password VARCHAR NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS games (
    id SERIAL,
    created_id INTEGER NOT NULL,
    other_id INTEGER NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (created_id) REFERENCES users (id),
    FOREIGN KEY (other_id) REFERENCES users (id)
);

CREATE TABLE IF NOT EXISTS moves (
    id SERIAL,
    game_id INTEGER NOT NULL,
    notation VARCHAR NOT NULL,
    FOREIGN KEY (game_id) REFERENCES games (id)
);
