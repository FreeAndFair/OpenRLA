export const mkUrl = path => `http://localhost:8080${path}`;

const checkStatus = r => {
  const isOk = 200 <= r.status && r.status < 300;
  if (isOk) {
    return Promise.resolve(r);
  } else {
    const err = new Error(r.statusText);
    return Promise.reject(err);
  }
};

export const fetch = path =>
  window.fetch(mkUrl(path))
  .then(checkStatus)
  .then(r => r.json());

export const submit = (path, data) =>
  window.fetch(mkUrl(path), {
    method: 'post',
    body: JSON.stringify(data),
  })
  .then(checkStatus)
  .then(r => r.json());

export const save = (path, data) =>
  window.fetch(mkUrl(path), {
    method: 'put',
    body: JSON.stringify(data),
  })
  .then(checkStatus)
  .then(r => r.json());

export const formatPercent
  = (val, digits) => `${(val * 100).toFixed(digits)} %`;
