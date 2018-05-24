defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV(filePath) do
    File.read!(filePath)
    |> String.split(["\r\n", "\n", "\r"])
    |> Enum.map(fn(x) -> String.split(x, [","]) end)
    |> Enum.map(&parseEntry/1)
  end

  def identifyStations(stations) do
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
