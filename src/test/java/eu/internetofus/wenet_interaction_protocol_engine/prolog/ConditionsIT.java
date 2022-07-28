/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.prolog;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Test the conditions.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ConditionsIT extends AbstractConditionsITC {

  /**
   * {@inheritDoc}
   *
   * @return {@code 2} in any case.
   */
  @Override
  protected int numberOfUsersToCreate() {

    return 2;
  }

  /**
   * Return the conditions that are {@code true} and has to be test.
   *
   * @return the list of condition that has to be {@code true}.
   */
  @Override
  public List<String> getSucessConditionsToTest() {

    final List<String> conditions = new ArrayList<>();
    conditions.add("timestamp_to_week_day(4,1)");

    final var nowWeekDay = LocalDate.now(ZoneId.of("Z")).getDayOfWeek();
    for (final var weekDay : DayOfWeek.values()) {

      final var condition1 = "is_now_on_week_day(" + weekDay.getValue() + ")";
      final var condition2 = "is_now_on_" + weekDay.name().toLowerCase() + "()";

      if (weekDay == nowWeekDay) {

        conditions.add(condition1);
        conditions.add(condition2);

      } else {

        conditions.add("not(" + condition1 + ")");
        conditions.add("not(" + condition2 + ")");
      }
    }

    final var nextWeekDay = nowWeekDay.plus(1).getValue();
    final var prevWeekDay = nowWeekDay.minus(1).getValue();
    conditions.add("is_now_one_of_week_days([" + prevWeekDay + "," + nowWeekDay.getValue() + "," + nextWeekDay + "])");
    conditions.add("not(is_now_one_of_week_days([" + prevWeekDay + "," + nextWeekDay + "]))");

    conditions.add("string_to_time(\"01:02\",'1:2:9')");
    conditions.add("string_to_time(\"01:02\",'1:2')");
    conditions.add("string_to_time(\"23:54\",'23:54')");
    conditions.add("string_to_time(\"23:54\",\"23:54\")");
    conditions.add("normalized_time(\"01:02\",'1:2:9')");
    conditions.add("normalized_time(\"01:02\",'1:2')");
    conditions.add("normalized_time(\"01:02\",\"1:2\")");
    conditions.add("normalized_time(\"23:54\",'23:54')");
    conditions.add("normalized_time(\"23:54\",\"23:54\")");
    conditions.add("timestamp_to_time(\"00:00\",0)");
    conditions.add("timestamp_to_time(\"00:01\",60)");
    conditions.add("timestamp_to_time(\"12:10\",1652962250)");
    conditions.add("timestamp_to_time(\"02:30\",165292250)");
    conditions.add("timestamp_to_time(\"22:56\",1292177)");

    final var now = LocalTime.now(ZoneId.of("Z"));
    final var lower = now.minusMinutes(1).format(DateTimeFormatter.ofPattern("HH:mm"));
    final var upper = now.plusMinutes(1).format(DateTimeFormatter.ofPattern("HH:mm"));
    conditions.add("is_now_before_time(" + upper + ")");
    conditions.add("is_now_before_time('" + upper + "')");
    conditions.add("is_now_before_time(\"" + upper + "\")");
    conditions.add("not(is_now_before_time(\"" + lower + "\"))");
    conditions.add("is_now_before_time_or_equals(" + upper + ")");
    conditions.add("is_now_before_time_or_equals('" + upper + "')");
    conditions.add("is_now_before_time_or_equals(\"" + upper + "\")");
    conditions.add("not(is_now_before_time_or_equals(\"" + lower + "\"))");
    conditions.add("is_now_after_time(" + lower + ")");
    conditions.add("is_now_after_time('" + lower + "')");
    conditions.add("is_now_after_time(\"" + lower + "\")");
    conditions.add("not(is_now_after_time(\"" + upper + "\"))");
    conditions.add("is_now_after_time_or_equals(" + lower + ")");
    conditions.add("is_now_after_time_or_equals('" + lower + "')");
    conditions.add("is_now_after_time_or_equals(\"" + lower + "\")");
    conditions.add("not(is_now_after_time_or_equals(\"" + upper + "\"))");
    conditions.add("is_now_between_times(" + lower + "," + upper + ")");
    conditions.add("is_now_between_times('" + lower + "'," + upper + ")");
    conditions.add("is_now_between_times(\"" + lower + "\"," + upper + ")");
    conditions.add("is_now_between_times(" + lower + ",'" + upper + "')");
    conditions.add("is_now_between_times(" + lower + ",\"" + upper + "\")");
    conditions.add("is_now_between_times('" + lower + "','" + upper + "')");
    conditions.add("is_now_between_times(\"" + lower + "\",\"" + upper + "\")");

    conditions.add("get_profile_language(\"" + this.users.get(0).locale.substring(0, 2) + "\")");

    conditions.add(
        "wenet_value_of_user_id_from_user_values(1.0,\"0\",[json([userId=\"0\",value=1.0]),json([userId=\"1\",value=0.0])],-1)");
    conditions.add(
        "wenet_value_of_user_id_from_user_values(0.0,\"1\",[json([userId=\"0\",value=1.0]),json([userId=\"1\",value=0.0])],-1)");
    conditions.add(
        "wenet_value_of_user_id_from_user_values(-1,\"2\",[json([userId=\"0\",value=1.0]),json([userId=\"1\",value=0.0])],-1)");

    return conditions;

  }

}
