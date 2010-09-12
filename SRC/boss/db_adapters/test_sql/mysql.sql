DROP TABLE IF EXISTS boss_db_test_models;

CREATE TABLE boss_db_test_models (
    id                  INTEGER AUTO_INCREMENT PRIMARY KEY,
    some_text           TEXT,
    some_time           DATETIME,
    some_boolean        BOOLEAN,
    some_integer        INTEGER,
    some_float          FLOAT
);

DROP TABLE IF EXISTS counters;

CREATE TABLE counters (
    name                VARCHAR(255) PRIMARY KEY,
    value               INTEGER DEFAULT 0
);
