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

import eu.internetofus.common.components.models.Message;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.personal_context_builder.UserDistance;
import eu.internetofus.common.components.personal_context_builder.UserLocation;
import eu.internetofus.common.components.personal_context_builder.WeNetPersonalContextBuilderSimulator;
import eu.internetofus.common.components.service.MessagePredicates;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.function.Predicate;

/**
 * Test the condition to calculate the normalized closeness.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ConditionNormalizedClosenessIT extends AbstractPrologITC {

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<TaskType> createTaskTypeForProtocol(final Vertx vertx, final VertxTestContext testContext) {

    return super.createTaskTypeForProtocol(vertx, testContext).map(taskType -> {

      taskType.callbacks = new JsonObject().put("result",
          new JsonObject().put("type", "object").put("properties", new JsonObject().put("closeness",
              new JsonObject().put("type", "array").put("items", new JsonObject().put("type", "object")))));
      return taskType;

    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getWheneverCode() {

    return "get_app_users_except_me(Users)";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getThenceforthCode() {

    return "normalized_closeness(Result,Users,10000) and send_user_message(\"result\",json([closeness=Result]))";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doBeforeTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    Future<?> future = Future.succeededFuture();
    for (var i = 0; i < this.users.size(); i++) {

      if (i % 2 == 0) {

        final var location = new UserLocation();
        location.userId = this.users.get(i).id;
        if (i % 3 == 0) {

          location.latitude = i;
          location.longitude = i;

        } else {

          location.latitude = i * 0.001;
          location.longitude = i * 0.001;
        }
        future = future
            .compose(ignored -> WeNetPersonalContextBuilderSimulator.createProxy(vertx).addUserLocation(location));

      }

    }
    return future;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doAfterTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    final var closeness = new JsonArray();
    for (var i = 1; i < this.users.size(); i++) {

      final var normalized = new JsonObject();
      normalized.put("userId", this.users.get(i).id);
      var distance = 0.0;
      if (i % 2 == 0 && i % 3 != 0) {

        distance = 1.0 - UserDistance.calculateDistance(0, 0, i * 0.001, i * 0.001) / 10000.0;
      }
      normalized.put("value", distance);
      closeness.add(normalized);

    }
    final var result = new JsonObject().put("closeness", closeness);
    final var checkMessages = new ArrayList<Predicate<Message>>();
    checkMessages.add(this.createMessagePredicate().and(MessagePredicates.labelIs("result"))
        .and(MessagePredicates.attributesAre(result)));

    return this.waitUntilCallbacks(vertx, testContext, checkMessages);
  }

}
