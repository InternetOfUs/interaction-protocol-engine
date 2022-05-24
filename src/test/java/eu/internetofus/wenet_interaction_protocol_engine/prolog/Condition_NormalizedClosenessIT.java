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

import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.personal_context_builder.UserDistance;
import eu.internetofus.common.components.personal_context_builder.UserLocation;
import eu.internetofus.common.components.personal_context_builder.WeNetPersonalContextBuilderSimulator;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.protocols.AbstractProtocolITC;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.tinylog.Logger;

/**
 * Test the normalized closeness condition.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class Condition_NormalizedClosenessIT extends AbstractProtocolITC {

  /**
   * The maximum distance.
   */
  private static final double MAX_DISTANCE = 100000.0;

  /**
   * The step to apply to set the distance of an user.
   */
  private static final double STEP = 0.075;

  /**
   * The maximum distance.
   */
  private static final double MAX_DOUBLE_COMPARE = 0.00000001;

  /**
   * {@inheritDoc}
   *
   * @return {@code 11} in any case.
   */
  @Override
  protected int numberOfUsersToCreate() {

    return 11;
  }

  /**
   * Create the task type to test.
   *
   * {@inheritDoc}
   */
  @Override
  protected Future<TaskType> createTaskTypeForProtocol(final Vertx vertx, final VertxTestContext testContext) {

    Future<TaskType> future = Future.succeededFuture(new TaskType());
    for (var i = 0; i < this.users.size(); i++) {

      final var userId = this.users.get(i).id;
      if (i % 2 == 0) {

        final var location = new UserLocation();
        location.userId = userId;
        location.latitude = STEP * i;
        location.longitude = STEP * i;
        future = future.compose(taskType -> WeNetPersonalContextBuilderSimulator.createProxy(vertx)
            .addUserLocation(location).map(any -> taskType));

      }

    }

    return future.map(taskType -> {

      taskType.transactions = new JsonObject().put("test", new JsonObject().put("type", "object").put("properties",
          new JsonObject().put("content", new JsonObject().put("type", "string"))));
      final var norm = new ProtocolNorm();
      norm.whenever = "get_app_users_except_me(Users) and normalized_closeness(Result,Users," + MAX_DISTANCE + ")";
      norm.thenceforth = "put_task_state_attribute(result,Result)";
      norm.priority = 0;
      taskType.norms = new ArrayList<>();
      taskType.norms.add(norm);
      final var createdTaskNorm = new ProtocolNorm();
      createdTaskNorm.priority = 1;
      createdTaskNorm.whenever = "is_received_created_task()";
      createdTaskNorm.thenceforth = "add_created_transaction()";
      taskType.norms.add(createdTaskNorm);
      return taskType;
    });
  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(5)
  public void shouldCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(4, testContext);

    final var source = this.createTaskForProtocol();

    final var createTransaction = new TaskTransaction();
    createTransaction.label = TaskTransaction.CREATE_TASK_LABEL;
    createTransaction.actioneerId = source.requesterId;
    final var checkTask = this.createTaskPredicate().and(TaskPredicates.similarTo(source))
        .and(TaskPredicates.transactionSizeIs(1)).and(TaskPredicates.transactionAt(0,
            this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(createTransaction))));

    this.waitUntilTaskCreated(source, vertx, testContext, checkTask)
        .compose(ignored -> this.waitUntilUserTaskState(source.requesterId, vertx, testContext, state -> {

          final var result = state.attributes.getJsonArray("result");
          if (result == null || result.size() != this.users.size() - 1) {

            return false;
          }
          for (var i = 0; i < result.size(); i++) {

            final var element = result.getJsonObject(i);
            final var userId = element.getString("userId");
            final var value = element.getDouble("value");
            var found = false;
            for (var j = 1; j < this.users.size() && !found; j++) {

              final var user = this.users.get(j);
              if (user.id.equals(userId)) {

                found = true;
                final var distance = this.normalizedDistance(0, 0, j);
                if (Math.abs(distance - value) > MAX_DOUBLE_COMPARE) {

                  Logger.debug("{} IS NOT EQUALS TO {}", distance, value);
                  return false;
                }
              }
            }

            if (!found) {
              // Not found user
              Logger.error("Not found user {}", userId);
              return false;
            }

          }

          return true;

        })).onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

  /**
   * Check that does not have distance if the reference user does not have
   * location.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(6)
  public void shouldNotBeNearIfReferenceDoesNotHaveLocation(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var testTransaction = new TaskTransaction();
    testTransaction.taskId = this.task.id;
    testTransaction.label = "test";
    testTransaction.attributes = new JsonObject().put("content", "shouldNotBeNearIfReferenceDoesNotHaveLocation");
    testTransaction.actioneerId = this.users.get(1).id;

    WeNetTaskManager.createProxy(vertx).doTaskTransaction(testTransaction)
        .compose(any -> this.waitUntilUserTaskState(testTransaction.actioneerId, vertx, testContext, state -> {

          final var result = state.attributes.getJsonArray("result");
          if (result == null || result.size() != this.users.size() - 1) {

            return false;
          }
          for (var i = 0; i < result.size(); i++) {

            final var element = result.getJsonObject(i);
            final var userId = element.getString("userId");
            if (!testTransaction.actioneerId.equals(userId)) {
              final var value = element.getDouble("value");
              var found = false;
              for (var j = 0; j < this.users.size() && !found; j++) {

                final var user = this.users.get(j);
                if (user.id.equals(userId)) {

                  found = true;
                  if (Math.abs(0 - value) > MAX_DOUBLE_COMPARE) {

                    Logger.debug("0 IS NOT EQUALS TO {}", value);
                    return false;
                  }
                }
              }

              if (!found) {
                // Not found user
                Logger.error("Not found user {}", userId);
                return false;
              }
            }
          }

          return true;

        })).onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));
  }

  /**
   * Check that obtain closeness for a non requester user.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(7)
  public void shouldObtainClosenessFromNoRequesterUser(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var testTransaction = new TaskTransaction();
    testTransaction.taskId = this.task.id;
    testTransaction.label = "test";
    testTransaction.attributes = new JsonObject().put("content", "shouldObtainClosenessFromNoRequesterUser");
    testTransaction.actioneerId = this.users.get(6).id;

    WeNetTaskManager.createProxy(vertx).doTaskTransaction(testTransaction)
        .compose(any -> this.waitUntilUserTaskState(testTransaction.actioneerId, vertx, testContext, state -> {

          final var result = state.attributes.getJsonArray("result");
          if (result == null || result.size() != this.users.size() - 1) {

            return false;
          }
          for (var i = 0; i < result.size(); i++) {

            final var element = result.getJsonObject(i);
            final var userId = element.getString("userId");
            if (!testTransaction.actioneerId.equals(userId)) {

              final var value = element.getDouble("value");
              var found = false;
              for (var j = 0; j < this.users.size() && !found; j++) {

                final var user = this.users.get(j);
                if (user.id.equals(userId)) {

                  found = true;
                  final var distance = this.normalizedDistance(6 * STEP, 6 * STEP, j);
                  if (Math.abs(distance - value) > MAX_DOUBLE_COMPARE) {

                    Logger.debug("{} IS NOT EQUALS TO {}", distance, value);
                    return false;
                  }
                }
              }

              if (!found) {
                // Not found user
                Logger.error("Not found user {}", userId);
                return false;
              }

            }
          }

          return true;

        })).onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));
  }

  /**
   * Calculate the normalized distance.
   *
   * @param sourceLongitude source longitude.
   * @param sourceLatitude  source latitude.
   * @param index           of the user.
   *
   * @return the normalized distance.
   */
  private double normalizedDistance(final double sourceLongitude, final double sourceLatitude, final int index) {

    if (index % 2 == 0) {
      return 1.0 - Math.min(MAX_DISTANCE,
          UserDistance.calculateDistance(sourceLongitude, sourceLatitude, STEP * index, STEP * index)) / MAX_DISTANCE;

    } else {

      return 0.0;
    }

  }

}
