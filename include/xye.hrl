-record(xye,
	{
	 id      = 1           :: 0..63,
	 monitor = true,

	 cap1    = 16#30,
	 cap2    = 16#10,
	 oper    = cool        :: off | auto | cool | dry | heat | fan,
	 fan     = auto        :: auto | high | medium | low,
	 temp    = 20          :: 16..32,
	 t1      = 16#5f,
	 t2a     = 16#4c,
	 t2b     = 16#ff,
	 t3      = 16#64,
	 current = 16#ff,
	 ff_03   = 16#ff,
	 on      = 0,         %%  0x01 - 15min, 0x02 - 30min, 0x04 - 1h, 0x08 - 2h, 0x10 - 4h, 0x20 - 8h, 0x40 - 16h  0x80 - invalid
	 off     = 0,         %%  0x01 - 15min, 0x02 - 30min, 0x04 - 1h, 0x08 - 2h, 0x10 - 4h, 0x20 - 8h, 0x40 - 16h  0x80 - invalid
	 run     = 1          :: 0 | 1,
	 mode    = normal     :: normal | turbo | sleep,
	 water   = 4          :: 0 | 4,
	 error   = 0,         %% E + bitpos (0 .. 0xF)
	 protect = 0,         %% P + bitpos (0 .. 0xF)
	 ccm     = 0,         %% CCM Comm State, bitpos ...
	 lock    = locked,
	 l1      = 0,
	 l2      = 0,
	 l3      = 0
	}).
