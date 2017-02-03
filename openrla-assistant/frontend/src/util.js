export const mkUrl = path => `http://localhost:8080${path}`;

export const fetch = path =>
  window.fetch(mkUrl(path))
  .then(r => r.json());

export const submit = (path, data) =>
  window.fetch(mkUrl(path), {
    method: 'post',
    body: JSON.stringify(data),
  }).then(r => r.json());
