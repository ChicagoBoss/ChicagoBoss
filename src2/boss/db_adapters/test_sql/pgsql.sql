DROP TABLE IF EXISTS boss_db_test_models;
CREATE TABLE boss_db_test_models (
    id                  SERIAL PRIMARY KEY,
    some_text           TEXT,
    some_time           TIMESTAMP,
    some_boolean        BOOLEAN,
    some_integer        INTEGER,
    some_float          FLOAT,
    boss_db_test_parent_model_id INTEGER
);

DROP TABLE IF EXISTS boss_db_test_parent_models;
CREATE TABLE boss_db_test_models (
    id                  SERIAL PRIMARY KEY,
    some_text           TEXT
);

DROP TABLE IF EXISTS counters;
CREATE TABLE counters (
    name                VARCHAR(255) PRIMARY KEY,
    value               INTEGER DEFAULT 0
);
