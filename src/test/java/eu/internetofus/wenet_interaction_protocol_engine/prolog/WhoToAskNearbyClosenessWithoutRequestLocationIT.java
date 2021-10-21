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

import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.personal_context_builder.UserLocation;
import eu.internetofus.common.components.personal_context_builder.WeNetPersonalContextBuilderSimulator;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the condition to calculate the normalized socialness.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WhoToAskNearbyClosenessWithoutRequestLocationIT extends AbstractWhoToAskITC {

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doBeforeTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    Future<?> future = Future.succeededFuture();
    for (var i = 1; i < this.users.size(); i++) {

      final var userId = this.users.get(i).id;
      final var location = new UserLocation();
      location.userId = userId;
      location.latitude = 0.4 * i;
      location.longitude = 0.4 * i;
      future = future
          .compose(ignored -> WeNetPersonalContextBuilderSimulator.createProxy(vertx).addUserLocation(location));

    }

    return future;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Task createTaskForProtocol() {

    final var task = super.createTaskForProtocol();
    task.attributes.put("positionOfAnswerer", "nearby").put("maxUsers", 5);
    return task;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doAfterTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    return this.waitUntilUserTaskState(this.task.requesterId, vertx, testContext, userTaskState -> {

      if (userTaskState.attributes != null) {

        final var closenessUsers = userTaskState.attributes.getJsonArray("closenessUsers");
        final var whoToAsk = userTaskState.attributes.getJsonArray("whoToAskUsers");
        for (var i = 1; i < this.users.size(); i++) {

          final var userId = this.users.get(i).id;
          var found = false;
          for (var j = 0; j < closenessUsers.size(); j++) {

            final var element = closenessUsers.getJsonObject(j);
            if (userId.equals(element.getString("userId"))) {

              found = Math.abs(element.getDouble("value")) < Double.MIN_NORMAL;
              break;
            }
          }
          if (!found) {

            return false;
          }

          for (var j = 0; j < whoToAsk.size(); j++) {

            final var element = whoToAsk.getJsonObject(j);
            if (userId.equals(element.getString("userId"))) {

              found = Math.abs(element.getDouble("value")) < Double.MIN_NORMAL;
              break;
            }
          }
          if (!found) {

            return false;
          }
        }

        final var unaskedUserIds = userTaskState.attributes.getJsonArray("unaskedUserIds");
        return unaskedUserIds != null && unaskedUserIds.size() == 4;
      }

      return false;

    });

  }

}
