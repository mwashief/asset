-- Active: 1700342331289@@127.0.0.1@5432@postgres

CREATE SCHEMA asset_management;

CREATE TABLE
    asset_management.asset(
        asset_id INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        asset_name VARCHAR(255) NOT NULL,
        unit VARCHAR(255) NOT NULL,
        asset_description VARCHAR(255)
    );

CREATE TABLE
    asset_management.asset_class(
        asset_class_id INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        asset_class_name VARCHAR(255) NOT NULL,
        asset_class_description VARCHAR(255)
    );

CREATE TABLE
    asset_management.asset_class_mapping(
        asset_class_id INT NOT NULL,
        asset_id INT NOT NULL,
        PRIMARY KEY (asset_class_id, asset_id),
        CONSTRAINT fk_asset_mapping FOREIGN KEY(asset_id) REFERENCES asset(asset_id),
        CONSTRAINT fk_asset_class_mapping FOREIGN KEY(asset_class_id) REFERENCES asset_class(asset_class_id)
    );

CREATE TABLE
    asset_management.category(
        category_id INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        category_name VARCHAR(255) NOT NULL,
        category_description VARCHAR(255)
    );

CREATE TABLE
    asset_management.asset_delta(
        asset_delta_id INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        date DATE NOT NULL,
        delta DOUBLE PRECISION NOT NULL,
        description VARCHAR(255),
        asset_id INT NOT NULL,
        category_id INT,
        CONSTRAINT fk_asset FOREIGN KEY(asset_id) REFERENCES asset(asset_id),
        CONSTRAINT fk_category FOREIGN KEY(category_id) REFERENCES category(category_id)
    );

CREATE TABLE
    asset_management.transaction(
        left_delta_id INT NOT NULL,
        right_delta_id INT NOT NULL,
        PRIMARY KEY (left_delta_id, right_delta_id),
        CONSTRAINT fk_left_delta_id FOREIGN KEY(left_delta_id) REFERENCES asset_delta(asset_delta_id),
        CONSTRAINT fk_right_delta_id FOREIGN KEY(right_delta_id) REFERENCES asset_delta(asset_delta_id)
    );

COMMENT ON TABLE asset_management.asset IS 'All the assest class';

COMMENT
    ON TABLE asset_management.asset_delta IS 'History of assest changing over time';

COMMENT
    ON COLUMN asset_management.asset_delta.delta IS 'What is the change of this asset';