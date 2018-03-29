# Copyright (c) 2011 Andrew Grieser, Tyler Wymer
# MIT license
# https://github.com/agrieser/fft_coffee

class Fft
  # Fft computes the the Fast Fourier Transform of data, using the Cooleyâ&#x20AC;&#x201C;Tukey
  # FFT algorithm. The Cooley-Turkey algorithm requires the fft_data's length
  # to be a power of 2, however this is handled automatically. You do NOT need
  # to pad the data manually.
  #
  # data - An array of data, alternating real and imaginary components. If you
  # only have real data (ie displacement data), you need to add 0's in between
  # your real data points. ie: [1,2,3,4] -> [1,0,2,0,3,0,4,0], or use a
  # preprocessor
  #
  # sample_rate - The rate at which data was sampled. The results given by
  # frequency() will have the same units as the sample rate. IE: Hz.
  #
  # preprocessor (optional) - This function will be called with the incoming
  # data as it's argument. You can use the preprocessor to insert 0's for the
  # imaginary component, convert to a 0 based average, or other signal cleanup.
  constructor: (data, @sample_rate, preprocessor) ->
    @amps = []
    if preprocessor
      @data = preprocessor(data.slice(0))
    else
      @data = data.slice(0)

  # Public: Get fft results.
  #
  # Returns: Array of results from last fft transform, alternates real and
  # imaginary components
  results: ->
    @data.slice(0)

  # Public: Runs fft in the forward direction. If you are uncertain if you want
  # forward or reverse, this is the one you want.
  #
  # Returns: A reference to this object (for method chaining)
  forward: ->
    @data = this.run_fft('forward')
    @

  # Public: Runs fft in the reverse direction.
  #
  # Returns: A reference to this object (for method chaining)
  reverse: ->
    @data = this.run_fft('reverse')
    @

  # Public: Compute the amplitudes from the real and imaginary components of the
  # last fft transformation.
  #
  # Note: The amplitudes of fft transformed data is mirrored about the
  # centerpoint. This function returns only the lower half of the amplitude
  # data.
  #
  # Returns: an array of amplitude data.
  amplitudes: ->
    return @amps if @amps.length > 0
    amps = []
    for i in [0...@data.length / 2] by 2
      amps.push Math.sqrt(Math.pow(@data[i], 2) + Math.pow(@data[i+1], 2))
    @amps = amps

  # Public: The frequencies of the bands for the transform.
  #
  # Returns: An array of frequencies corresponding to the bands of the fft tranform.
  frequencies: ->
    (this.frequency(band) for band in [0..this.amplitudes().length])

  # Public: Compute the frequency of an element from the amplitude array.
  #
  # Note: forward or reverse must have been called prior to using this function.
  #
  # Returns: The frequency of the amplitude data corresponding to index 'band'.
  frequency: (band) ->
    width = @sample_rate / (@data.length / 2)
    width * band

  # Public: Compute the amplitude of a given frequency.
  #
  # Note: forward or reverse must have been called prior to using this function.
  #
  # Returns: The amplitude at the given frequency.
  amplitude: (frequency) ->
    width = @sample_rate / (@data.length / 2)
    band = Math.floor(frequency / width)
    amps = this.amplitudes()
    amps[band]

  # Public: Compute the primary frequency.
  #
  # Note: forward or reverse must have been called prior to using this function.
  #
  # Returns: The frequency with the highest amplitude (the primary frequency).
  primary_frequency: ->
    amps = this.amplitudes()
    max = amps[0]
    max_index = 0
    for i in [0...amps.length]
      if amps[i] > max
        max = amps[i]
        max_index = i
    this.frequency(max_index)


  # Private
  run_fft: (direction) ->
    @amps = []
    if direction == 'reverse'
      isign = -1
    else
      isign = 1

    this.pad(@data)
    this.fourier_transform(@data, @data.length / 2, isign)

    if direction == 'reverse'
      (elem / (@data.length / 2) for elem in @data)
    else
      @data

  # Private
  #
  # Core FFT implementation from:
  # Numerical Recipes: The Art of Scientific Computing
  # Third Edition (2007)
  # ISBN-10: 0521880688
  # http://www.nr.com/
  fourier_transform: (data, n, isign) ->
    self = @
    nn = n << 1
    j = 1

    for i in [1...nn] by 2
      if (j > i)
        self.swap(data, j-1, i-1)
        self.swap(data, j, i)
      m = n

      while(m >= 2 && j > m)
        j -= m
        m >>= 1
      j += m

    mmax = 2
    while nn > mmax
      istep = mmax << 1
      theta = isign*(Math.PI*2/mmax)
      wtemp = Math.sin(0.5 * theta)
      wpr = -2.0 * wtemp * wtemp
      wpi = Math.sin(theta)
      wr = 1.0
      wi = 0.0
      for m in [1...mmax] by 2
        for i in [m..nn] by istep
          j = i + mmax
          tempr = wr * data[j-1] - wi * data[j]
          tempi = wr * data[j] + wi * data[j-1]
          data[j-1] = data[i-1] - tempr
          data[j] = data[i] - tempi
          data[i-1] += tempr
          data[i] += tempi
        wr = (wtemp = wr) * wpr - wi * wpi + wr
        wi = wi * wpr + wtemp * wpi + wi
      mmax = istep

  # Private
  pad: (data) ->
    next_power = Math.ceil(Math.log(data.length) / Math.log(2))
    extended_length = Math.pow(2, next_power)
    while data.length < extended_length
      data.push 0

  # Private
  swap: (data, i, j) ->
    temp = data[i]
    data[i] = data[j]
    data[j] = temp


# Node/browser compatibility
if window?
  window.Fft = Fft
else
  module.exports = Fft
