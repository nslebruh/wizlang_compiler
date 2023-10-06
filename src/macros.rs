mod short_hand {
  #[macro_export]
  macro_rules! if_or {
    ($var:expr, $pred:expr, $($preds2:expr),+) => {
        $($var == $preds2 || )+$var == $pred
    };
  }
  #[macro_export]
  macro_rules! and_or {
    ($var:expr, $pred:expr, $($preds2:expr),+) => {
        $($var == $preds2 && )+$var == $pred
    };
  }
}
