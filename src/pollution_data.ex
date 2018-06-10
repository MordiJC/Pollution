defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV(filePath) do
    File.read!(filePath)
    |> String.split(["\r\n", "\n", "\r"])
    |> Enum.map(fn(x) -> String.split(x, [","]) end)
    |> Enum.map(&parseEntry/1)
  end

  def importFromCSV(filePath) do
    entries = importLinesFromCSV(filePath)
    stations = identifyStations(entries)
    importStations(stations)
    importMeasurements(entries, "PM10")
  end

  def importStations(stations) do
    for station <- stations, do:
      :pollution_server_otp.addStation(station.name, station.location)
  end

  def importMeasurements(entries, type) do
    for entry <- entries, do:
      :pollution_server_otp.addValue(entry.location, entry.datetime, type, entry.pollutionLevel)
  end

  def importWithMeasurements(filePath) do
    entries = importLinesFromCSV(filePath)
    stations = identifyStations(entries)
    %{
      :stationsLoadingTime => measure(fn -> importStations(stations) end),
      :measurementsLoadingTime => measure(fn -> importMeasurements(entries, "PM10") end)
    }
  end

  defp identifyStations(stations) do
    stations
    |> Enum.uniq_by(fn(m) -> m.location end)
#    |> Enum.reduce(0, fn (_x, acc) -> acc + 1 end)
    |> Enum.map(fn(m) -> %{:name => "station_#{elem(m.location, 0)}_#{elem(m.location, 1)}",
                         :location => m.location} end)
  end

  defp parseEntry([date, time, lat, lon, value]) do
    %{
    :datetime => {stringToDate(date), stringToTime(time)},
    :location => {String.to_float(lat), String.to_float(lon)},
    :pollutionLevel => String.to_integer(value)
    }
  end

  defp stringToDate(date) do
    String.split(date, "-")
    |> Enum.map(fn(n) -> String.to_integer(n) end)
    |> Enum.reverse
    |> List.to_tuple
  end

  defp stringToTime(time) do
    (String.split(time, ":")
    |> Enum.map(&String.to_integer/1)
    |> Enum.to_list)
    ++ [ 0 ]
    |> List.to_tuple
  end

end
