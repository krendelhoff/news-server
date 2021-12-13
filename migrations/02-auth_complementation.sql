ALTER TABLE auth ADD COLUMN privileged bool
               , RENAME COLUMN created_at TO expires;
