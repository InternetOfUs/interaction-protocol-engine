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

import eu.internetofus.common.components.models.RelevantLocation;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.personal_context_builder.UserLocation;
import eu.internetofus.common.components.personal_context_builder.WeNetPersonalContextBuilderSimulator;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

/**
 * Test the conditions associated to locations.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class LocationConditionsIT extends AbstractConditionsITC {

  /**
   * The step to apply to set the distance of an user.
   */
  private static final double STEP = 0.01;

  /**
   * {@inheritDoc}
   *
   * @return {@code 12} in any case.
   */
  @Override
  protected int numberOfUsersToCreate() {

    return 12;
  }

  /**
   * Add locations to the users.
   *
   * {@inheritDoc}
   */
  @Override
  @Test
  @Order(5)
  public void shouldPrepareEnvironmentToCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(4, testContext);

    Future<?> future = Future.succeededFuture(new TaskType());
    for (var i = 0; i < this.users.size(); i++) {

      final var userId = this.users.get(i).id;
      final var location = new UserLocation();
      location.userId = userId;
      location.latitude = STEP * i;
      location.longitude = STEP * i;
      future = future.compose(any -> WeNetPersonalContextBuilderSimulator.createProxy(vertx).addUserLocation(location));

    }

    this.users.get(0).relevantLocations = new ArrayList<>();
    final var homeLocation = new RelevantLocation();
    homeLocation.id = "0";
    homeLocation.label = "Home";
    homeLocation.longitude = 0.001;
    homeLocation.latitude = 0.001;
    this.users.get(0).relevantLocations.add(homeLocation);

    final var workLocation = new RelevantLocation();
    workLocation.id = "1";
    workLocation.label = "Work";
    workLocation.longitude = 0.03;
    workLocation.latitude = 0.03;
    this.users.get(0).relevantLocations.add(workLocation);

    future = future.compose(any -> WeNetProfileManager.createProxy(vertx).updateProfile(this.users.get(0)));
    future.onFailure(error -> testContext.failNow(error)).onSuccess(any -> this.assertSuccessfulCompleted(testContext));

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getSucessConditionsToTest() {

    final List<String> conditions = new ArrayList<>();

    conditions.add("get_closest_users_to_me(['" + this.users.get(1).id + "','" + this.users.get(2).id + "','"
        + this.users.get(3).id + "','" + this.users.get(4).id + "','" + this.users.get(5).id + "'])");
    conditions.add("get_app_users_near_me(['" + this.users.get(2).id + "','" + this.users.get(3).id + "','"
        + this.users.get(4).id + "'],2000,7000)");

    conditions.add("get_current_location(0.0,0.0)");

    conditions.add("is_current_location_near(0,0)");
    conditions.add("not(is_current_location_near(10,10))");
    conditions.add("is_current_location_near(0.04,0.04,7000)");
    conditions.add("not(is_current_location_near(0.04,0.04,6000))");

    conditions.add("not(is_current_location_near_relevant('Undefined label'))");
    conditions.add("not(is_current_location_near_relevant('Undefined label',100000))");
    conditions.add("is_current_location_near_relevant('0')");
    conditions.add("is_current_location_near_relevant('Home')");
    conditions.add("is_current_location_near_relevant('Home',500)");
    conditions.add("not(is_current_location_near_relevant('1'))");
    conditions.add("not(is_current_location_near_relevant('Work'))");
    conditions.add("not(is_current_location_near_relevant('Work',3000))");
    conditions.add("is_current_location_near_relevant('Work',6000)");

    return conditions;

  }

}
