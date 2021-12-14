ALTER TABLE auth ADD COLUMN privileged bool;
ALTER TABLE auth RENAME created_at TO expires;
