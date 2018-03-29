/*
 * Implementation file for Novint Falcon Max/Pd External
 *
 * Copyright (c) 2005-2010 Kyle Machulis/Nonpolynomial Labs <kyle@nonpolynomial.com>
 *
 * More info on Nonpolynomial Labs @ http://www.nonpolynomial.com
 *
 * Sourceforge project @ http://www.github.com/qdot/np_nifalcon
 *
 * Example code from flext tutorials. http://www.parasitaere-kapazitaeten.net/ext/flext/
 */


#ifndef FLEXT_THREADS
#define FLEXT_THREADS
#endif

// include flext header
#include <flext.h>

//god damnit, cycling '74.
#ifdef PI
#undef PI
#endif

#include "falcon/core/FalconDevice.h"
#include "falcon/grip/FalconGripFourButton.h"
#include "falcon/kinematic/FalconKinematicStamper.h"
#include "falcon/firmware/FalconFirmwareNovintSDK.h"
#include "falcon/util/FalconFirmwareBinaryNvent.h"

// check for appropriate flext version
#if !defined(FLEXT_VERSION) || (FLEXT_VERSION < 400)
#error You need at least flext version 0.4.0
#endif

// implementation file, just use the namespace and make life easier.
using namespace libnifalcon;

class np_nifalcon:
	// inherit from basic flext class
	public flext_base
{
	// obligatory flext header (class name,base class name)
	FLEXT_HEADER(np_nifalcon,flext_base)

	// Same as boost ScopedMutex, just using flext's mutex class.
	class ScopedMutex
	{
		ScopedMutex() {}

	public:
		ScopedMutex(ThrMutex& tm)
		{
			m = &tm;
			m->Lock();
		}

		~ScopedMutex()
		{
			m->Unlock();
		}
	private:
		ThrMutex* m;
	};

public:
	// constructor
	np_nifalcon() :
		m_deviceIndex(-1),
		m_runThread(false),
		m_isInited(false),
		m_inRawMode(true),
		m_ledState(0),
		m_homingMode(false),
		m_hasUpdated(false),
		m_alwaysOutput(false),
		motor_changed(false),
		coordinate_changed(false),
		homing_state(false),
		button_state(0),
		sleep_time(.001)
	{
		// object setup

		for(int i = 0; i < 3; ++i)
		{
			m_motorVectorForce[i] = 0;
			m_motorRawForce[i] = 0;
		}

		m_falconDevice.setFalconFirmware<FalconFirmwareNovintSDK>();
		m_falconDevice.setFalconGrip<FalconGripFourButton>();
		m_falconDevice.setFalconKinematic<FalconKinematicStamper>();

		// external setup
		AddInAnything("Command Input");
		AddInList("Motor - Raw input to motors (3 ints)");
		AddInList("Motor - Forces as cartesian vector (3 floats)");
		AddInList("LEDS (RGB, 3 Ints)");
		AddInInt("Homing Mode");
		AddOutBang("Bangs on successful connection/command");
		AddOutList("Encoder Values (Top, right, left when looking at front of falcon)");
		AddOutList("Cartesian Position (X, Y, Z of the end effector in relation to the fixed frame origin)");
		AddOutList("Buttons - Digital");
		AddOutList("Button - Analog");
		AddOutInt("Homing Status");

		FLEXT_ADDBANG(0, nifalcon_output_request);
		FLEXT_ADDMETHOD_(0, "open", nifalcon_anything);
		FLEXT_ADDMETHOD_(0, "init", nifalcon_anything);
		FLEXT_ADDMETHOD_(0, "sleep_time", nifalcon_anything);

		FLEXT_ADDMETHOD_(0, "auto_poll", nifalcon_auto_poll);
		FLEXT_ADDMETHOD_(0, "manual_poll", nifalcon_manual_poll);
		FLEXT_ADDMETHOD_(0, "start", nifalcon_start_thread);
		FLEXT_ADDMETHOD_(0, "update", nifalcon_update_loop);
		FLEXT_ADDMETHOD_(0, "output", nifalcon_output_request);
		FLEXT_ADDMETHOD_(0, "count", nifalcon_count);
		FLEXT_ADDMETHOD_(0, "initialize", nifalcon_nvent_firmware);
		FLEXT_ADDMETHOD_(0, "nvent_firmware", nifalcon_nvent_firmware);
		FLEXT_ADDMETHOD_(0, "close", nifalcon_close);
		FLEXT_ADDMETHOD_(0, "stop", nifalcon_stop);
		FLEXT_ADDMETHOD_(0, "raw", nifalcon_raw);
		FLEXT_ADDMETHOD_(0, "vector", nifalcon_vector);

		FLEXT_ADDMETHOD(1, nifalcon_motor_raw);
		FLEXT_ADDMETHOD(2, nifalcon_motor_vector);
		FLEXT_ADDMETHOD(3, nifalcon_led);
		FLEXT_ADDMETHOD(4, nifalcon_homing_mode);

		post("Novint Falcon External v1.6.2");
		post("by Nonpolynomial Labs (http://www.nonpolynomial.com)");
		post("Updates at http://www.github.com/qdot/np_nifalcon");
		post("Compiled on " __DATE__ " " __TIME__);

	}

	virtual void Exit()
	{
		m_alwaysOutput = false;
		if(m_runThread) nifalcon_stop();
		m_falconDevice.close();
		flext_base::Exit();
	}

	virtual ~np_nifalcon()
	{
		m_alwaysOutput = false;
		if(m_runThread) nifalcon_stop();
		m_falconDevice.close();
	}

protected:
	FalconDevice m_falconDevice;
	int m_deviceIndex;
	bool m_inRawMode;
	bool m_isInited;
	int m_ledState;
	boost::array<double,3> m_motorVectorForce;
	boost::array<int,3> m_motorRawForce;
	bool m_homingMode;

	bool m_runThread;
	ThrMutex m_deviceMutex;
	ThrMutex m_updateMutex;
	ThrMutex m_ioMutex;
	ThrMutex m_runMutex;
	bool m_hasUpdated;
	bool m_alwaysOutput;

	t_atom motor_list[3];
	t_atom coordinate_list[3];
	t_atom button_list[4];
	bool motor_changed;
	bool coordinate_changed;
	bool homing_state_changed;
	bool button_state_changed;
	bool old_homing_state;
	bool old_button_state;
	bool homing_state;
	uint8_t button_state;
	double sleep_time;
	
	void nifalcon_output_request()
	{
		if(!m_alwaysOutput)
		{
			nifalcon_output();
			return;
		}
		post("np_nifalcon: Cannot request output by bang/'output' message when auto_poll is on");
	}

	void nifalcon_count()
	{
		ScopedMutex s(m_deviceMutex);
		unsigned int count;
		if(!m_falconDevice.getDeviceCount(count))
		{
			post("np_nifalcon: Error while counting devices");
			return;
		}
		post("np_nifalcon: Falcons Available: %d", count);
	}

	void nifalcon_auto_poll()
	{
		post("np_nifalcon %d: Turning on auto polling mode", m_deviceIndex);
		m_alwaysOutput = true;
	}

	void nifalcon_manual_poll()
	{
		post("np_nifalcon %d: Turning on manual polling mode", m_deviceIndex);
		m_alwaysOutput = false;
	}

	void nifalcon_raw()
	{
		post("np_nifalcon %d: Falcon force input now in raw mode", m_deviceIndex);
		m_inRawMode = true;
	}

	void nifalcon_vector()
	{
		post("np_nifalcon %d: Falcon force input now in vector mode", m_deviceIndex);
		m_inRawMode = false;
	}

	void nifalcon_nvent_firmware()
	{
		if(!m_falconDevice.isOpen())
		{
			post("np_nifalcon: Falcon not open");
			return;
		}
		if(m_falconDevice.isFirmwareLoaded())
		{
			m_isInited = true;
			post("np_nifalcon %d: Firmware already loaded, no need to reload...", m_deviceIndex);
			return;
		}
		ScopedMutex s(m_deviceMutex);
		for(int i = 0; i < 10; ++i)
		{
			if(!m_falconDevice.getFalconFirmware()->loadFirmware(false, NOVINT_FALCON_NVENT_FIRMWARE_SIZE, const_cast<uint8_t*>(NOVINT_FALCON_NVENT_FIRMWARE)))
			{
				//Completely close and reopen
				m_falconDevice.close();
				if(!m_falconDevice.open(m_deviceIndex))
				{
					post("np_nifalcon: Cannot open falcon device index %d - Lib Error Code: %d Device Error Code: %d", m_deviceIndex, m_falconDevice.getErrorCode(),  m_falconDevice.getFalconComm()->getDeviceErrorCode());
					break;
				}
			}
			else
			{
				m_isInited = true;
				break;
			}
		}
		if(m_isInited)
			post("np_nifalcon %d: loading nvent firmware finished", m_deviceIndex);
		else
			post("np_nifalcon %d: loading nvent firmware FAILED", m_deviceIndex);
	}

	void nifalcon_close()
	{
		if(!m_falconDevice.isOpen())
		{
			post("np_nifalcon: Falcon not open");
			return;
		}
		m_isInited = false;
		if(m_runThread) nifalcon_stop();
		//ScopedMutex s(m_deviceMutex);
		m_falconDevice.close();
		post("np_nifalcon %d: Falcon device closed", m_deviceIndex);
		m_deviceIndex = -1;
		return;
	}

	void nifalcon_anything(const t_symbol *msg,int argc,t_atom *argv)
	{
		if(!strcmp(msg->s_name, "open"))
		{
			if(m_falconDevice.isOpen())
			{
				post("np_nifalcon %d: Falcon already open", m_deviceIndex);
				return;
			}
			ScopedMutex s(m_deviceMutex);
			int index = -1;
			if(argc == 1) index = GetInt(argv[0]);
			else index = 0;
			post("np_nifalcon: Opening first falcon found");
			if(!m_falconDevice.open(index))
			{
				post("np_nifalcon: Cannot open falcon %d - Error: %d", index, m_falconDevice.getErrorCode());
				return;
			}
			m_deviceIndex = index;
			post("np_nifalcon %d: Opened", m_deviceIndex);
			return;
		}
		else if (!strcmp(msg->s_name, "init"))
		{
			if(!m_falconDevice.isOpen())
			{
				post("np_nifalcon: Falcon not open");
				return;
			}
			if(m_falconDevice.isFirmwareLoaded())
			{
				m_isInited = true;
				post("np_nifalcon %d: Firmware already loaded, no need to reload...", m_deviceIndex);
				return;
			}
			if(!m_falconDevice.setFirmwareFile(GetString(argv[0])))
			{
				post("np_nifalcon %d: Cannot find firmware file %s", m_deviceIndex, GetString(argv[0]));
				return;
			}
			ScopedMutex s(m_deviceMutex);
			if(!m_falconDevice.loadFirmware(10, false))
			{
				post("np_nifalcon %d: Could not load firmware: %d", m_deviceIndex, m_falconDevice.getErrorCode());
				return;
			}
			m_isInited = true;
			post("np_nifalcon %d: Falcon init finished", m_deviceIndex);
		}
		else if(!strcmp(msg->s_name, "sleep_time"))
		{
			//We take microseconds as arguments. Not sure if we have that level of granularity, but it never hurts to try.
			if(argc == 1)
			{
				sleep_time = (double)GetInt(argv[0]) / (double)1000000;
				post("Set sleep time to %f", sleep_time);
			}
			else
			{
				post("np_nifalcon: sleep_usec message takes one argument (microseconds to sleep in polling thread)");
			}
		}		
		else
		{
			post("np_nifalcon: not a valid np_nifalcon message: %s", msg->s_name);
		}
	}

	void nifalcon_stop()
	{
		if(!m_falconDevice.isOpen())
		{
			post("np_nifalcon: Falcon not open");
			return;
		}
		if(!m_runThread)
		{
			post("np_nifalcon %d: No thread running", m_deviceIndex);
			return;
		}
		bool ao = m_alwaysOutput;
		m_alwaysOutput = false;
		m_runThread = false;
		//ScopedMutex r(m_runMutex);
		post("np_nifalcon %d: Input thread stopped", m_deviceIndex);
		m_alwaysOutput = ao;
		return;
	}

	void nifalcon_update_loop()
	{
		bool ret = false;

		//Run the IO Loop, locking the device while we do
		{
			//ScopedMutex m(m_deviceMutex);
			ret = m_falconDevice.runIOLoop(FalconDevice::FALCON_LOOP_FIRMWARE | FalconDevice::FALCON_LOOP_GRIP | (m_inRawMode ? 0 : FalconDevice::FALCON_LOOP_KINEMATIC));
		}

		//If we didn't get anything out of the IO loop, return
		if(!ret)
		{
			return;
		}

		//Put together the information
		{
			//lock to make sure we don't try to bang out half updated information
			//ScopedMutex m(m_ioMutex);
			int i = 0, buttons = 0;
			//t_atom analog_list[];
			//Output encoder values
			for(i = 0; i < 3; ++i)
			{
				if(GetInt(motor_list[i]) == (m_falconDevice.getFalconFirmware()->getEncoderValues())[i]) continue;
				motor_changed = true;
				SetInt(motor_list[i], m_falconDevice.getFalconFirmware()->getEncoderValues()[i]);
			}

			//Output kinematics values
			for(i = 0; i < 3; ++i)
			{
				if(fabs(GetFloat(coordinate_list[i]) - m_falconDevice.getPosition()[i]) < .000001 ) continue;
				coordinate_changed = true;
				SetFloat(coordinate_list[i], m_falconDevice.getPosition()[i]);
			}

			//Output digital values
			buttons = m_falconDevice.getFalconGrip()->getDigitalInputs();
			if(button_state != buttons)
			{
				for(i = 0; i < 4; ++i)
				{
					SetInt(button_list[i], buttons & (1 << i));
				}
				button_state = buttons;
				button_state_changed = true;
			}

			//Output analog values
			//We don't have analog values yet. Nothing will leave this output until I figured out analog. Implement later.

			//Output homing values
			if(m_falconDevice.getFalconFirmware()->isHomed() != homing_state)
			{
				homing_state = m_falconDevice.getFalconFirmware()->isHomed();
				homing_state_changed = true;
			}

			//Confirm that we're ready to output if we have something new
			if(motor_changed || coordinate_changed || button_state_changed || homing_state_changed)
				m_hasUpdated = true;
		}
		//If we're autopolling and we got this far, output
		if(m_alwaysOutput && m_hasUpdated) nifalcon_output();

		//Update the device with the information from the patch
		{
			//lock to make sure we don't try to update information from a patch while it's written to the device object
			//ScopedMutex t(m_updateMutex);
			//Now that we're done parsing what we got back, set the new internal values
			if(!m_inRawMode)
			{
				m_falconDevice.setForce(m_motorVectorForce);
			}
			else
			{
				m_falconDevice.getFalconFirmware()->setForces(m_motorRawForce);
			}

			m_falconDevice.getFalconFirmware()->setHomingMode(m_homingMode);

			m_falconDevice.getFalconFirmware()->setLEDStatus(m_ledState);
		}
	}

	void nifalcon_output()
	{
		//If we haven't run a new successful loop yet, we have nothing to output
		if(!m_hasUpdated) return;

		//Since this is the only function we output in, do a system level lock
		//This calls critical_enter/exit on max and sys_lock on Pd
		//Thanks to ClaudiusMaximus on freenode #dataflow for pointing this out.
		//See also http://lists.puredata.info/pipermail/pd-list/2005-01/025473.html

		//ONLY NEEDED FOR PD. LOCKS MAX
		//Lock();
		
		//Make sure we don't collide with the I/O loop if we're manually polling
		ScopedMutex s(m_ioMutex);
		if(motor_changed)
		{
			motor_changed = false;
			ToOutList(1, 3, motor_list);
		}
		if(coordinate_changed && !m_inRawMode)
		{
			coordinate_changed = false;
			ToOutList(2, 3, coordinate_list);
		}
		if(button_state_changed)
		{
			button_state_changed = false;
			ToOutList(3, 4, button_list);
		}
		if(homing_state_changed)
		{
			homing_state_changed = false;
			ToOutInt(5, homing_state);
		}
		ToOutBang(0);

		//ONLY NEEDED FOR PD. LOCKS MAX
		//Unlock();
		
		m_hasUpdated = false;
	}

	void nifalcon_start_thread()
	{
		if(!m_falconDevice.isOpen())
		{
			post("np_nifalcon: Falcon not open");
			return;
		}
		if(m_runThread)
		{
			post("np_nifalcon %d: Thread already running", m_deviceIndex);
			return;
		}
		if(!m_isInited)
		{
			post("np_nifalcon %d: Falcon must be initialized to start", m_deviceIndex);
			return;
		}
		ScopedMutex r(m_runMutex);
		m_runThread = true;
		Lock();
		post("np_nifalcon %d: Input thread started", m_deviceIndex);
		Unlock();
		while(m_runThread && m_falconDevice.isOpen())
		{
			nifalcon_update_loop();
			Sleep(sleep_time);
		}
		Lock();
		post("np_nifalcon %d: Input thread exiting", m_deviceIndex);
		Unlock();
	}

	void nifalcon_motor_raw(int motor_1, int motor_2, int motor_3)
	{
		ScopedMutex s(m_updateMutex);
		if(!m_inRawMode)
		{
			post("np_nifalcon: Falcon in vector force mode, raw input ignored");
			return;
		}
		m_motorRawForce[0] = motor_1;
		m_motorRawForce[1] = motor_2;
		m_motorRawForce[2] = motor_3;

	}

	void nifalcon_motor_vector(float x, float y, float z)
	{
		ScopedMutex s(m_updateMutex);
		if(m_inRawMode)
		{
			post("np_nifalcon: Falcon in raw mode, vector force input ignored");
			return;
		}
		m_motorVectorForce[0] = x;
		m_motorVectorForce[1] = y;
		m_motorVectorForce[2] = z;
	}

	void nifalcon_led(int red, int green, int blue)
	{
		ScopedMutex s(m_updateMutex);
		if(red > 0) m_ledState |= FalconFirmware::RED_LED;
		else m_ledState &= ~FalconFirmware::RED_LED;
		if(green > 0) m_ledState |= FalconFirmware::GREEN_LED;
		else m_ledState &= ~FalconFirmware::GREEN_LED;
		if(blue > 0) m_ledState |= FalconFirmware::BLUE_LED;
		else m_ledState &= ~FalconFirmware::BLUE_LED;
	}

	void nifalcon_homing_mode(long t)
	{
		ScopedMutex s(m_updateMutex);
		m_homingMode = (t > 0);
	}

private:
	FLEXT_CALLBACK_A(nifalcon_anything)
	FLEXT_CALLBACK(nifalcon_raw)
	FLEXT_CALLBACK(nifalcon_output_request)
	FLEXT_CALLBACK(nifalcon_vector)
	FLEXT_CALLBACK(nifalcon_manual_poll)
	FLEXT_CALLBACK(nifalcon_auto_poll)
	FLEXT_CALLBACK(nifalcon_count)
	FLEXT_CALLBACK(nifalcon_nvent_firmware)
	FLEXT_CALLBACK(nifalcon_stop)
	FLEXT_CALLBACK(nifalcon_close)
	FLEXT_CALLBACK(nifalcon_update_loop)
	FLEXT_CALLBACK(nifalcon_output)
	FLEXT_CALLBACK_III(nifalcon_motor_raw)
	FLEXT_CALLBACK_FFF(nifalcon_motor_vector)
	FLEXT_CALLBACK_III(nifalcon_led)
	FLEXT_CALLBACK_I(nifalcon_homing_mode)
	FLEXT_THREAD(nifalcon_start_thread)
};

FLEXT_NEW("np_nifalcon", np_nifalcon)



