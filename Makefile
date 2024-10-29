run: *
	cargo build
	ICED_BACKEND=tiny-skia RUST_BACKTRACE=1 target/debug/qp
