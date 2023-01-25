export const requestPermission_ = (onError, onSuccess) => {
  Notification.requestPermission().then((permission) => {
    onSuccess(permission);
  }).catch((errorValue) => {
    onError(errorValue);
  });

  return (_cancelError, _onCancelerError, onCancelerSuccess) => {
    onCancelerSuccess();
  };
};

