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
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.personal_context_builder.UserDistance;
import eu.internetofus.common.components.personal_context_builder.UserLocation;
import eu.internetofus.common.components.personal_context_builder.WeNetPersonalContextBuilderSimulator;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

/**
 * Test the condition to calculate the normalized socialness.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WhoToAskNearbyClosenessIT extends AbstractWhoToAskITC {

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doBeforeTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    Future<?> future = Future.succeededFuture();
    for (var i = 0; i < this.users.size(); i++) {

      final var userId = this.users.get(i).id;
      final var location = new UserLocation();
      location.userId = userId;
      location.latitude = 0.3 * i;
      location.longitude = 0.3 * i;
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
    task.attributes.put("positionOfAnswerer", "nearby").put("maxUsers", 3);
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
        for (var i = this.users.size() - 1; i > 0; i--) {

          final var userId = this.users.get(i).id;
          final var value = 1.0 - UserDistance.calculateDistance(0, 0, 0.3 * i, 0.3 * i) / 1000000.0;
          var found = false;
          for (var j = 0; j < closenessUsers.size(); j++) {

            final var element = closenessUsers.getJsonObject(j);
            if (userId.equals(element.getString("userId"))) {

              found = Math.abs(element.getDouble("value") - value) < Double.MIN_NORMAL;
              break;
            }
          }
          if (!found) {

            return false;
          }
        }

        final var whoToAsk = userTaskState.attributes.getJsonArray("whoToAskUsers");
        for (var i = 1; i < this.users.size(); i++) {

          final var userId = this.users.get(i).id;
          final var value = 1.0 - UserDistance.calculateDistance(0, 0, 0.3 * i, 0.3 * i) / 1000000.0;
          final var element = whoToAsk.getJsonObject(i - 1);
          if (!userId.equals(element.getString("userId"))
              && Math.abs(element.getDouble("value") - value) < Double.MIN_NORMAL) {

            return false;
          }
        }

        final var unaskedUserIds = userTaskState.attributes.getJsonArray("unaskedUserIds");
        for (var i = 4; i < this.users.size(); i++) {

          final var userId = this.users.get(i).id;
          if (!userId.equals(unaskedUserIds.getString(i - 4))) {

            return false;
          }
        }

        return true;
      }

      return false;

    }).compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, this.users.get(1),
        this.users.get(2), this.users.get(3)));

  }

  /**
   * Check that can ask to more users.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(6)
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  public void shouldAskMoreUsers(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var moreAnswerTransaction = new TaskTransaction();
    moreAnswerTransaction.taskId = this.task.id;
    moreAnswerTransaction.actioneerId = this.task.requesterId;
    moreAnswerTransaction.label = "moreAnswerTransaction";

    final var checkTask = this.createTaskPredicate()
        .and(TaskPredicates.lastTransactionIs(
            this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(moreAnswerTransaction))
                .and(TaskTransactionPredicates.messagesSizeIs(1))));
    WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction)
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(2))))
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, this.users.get(4),
            this.users.get(5), this.users.get(6)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(3))))
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, this.users.get(7),
            this.users.get(8), this.users.get(9)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(4))))
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, new WeNetUserProfile[0]))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(5))))
        .compose(ignored -> this.waitUntilResultContainsUsers(vertx, testContext, true, new WeNetUserProfile[0]))
        .onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

}
